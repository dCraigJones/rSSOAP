load("C:/Users/Craig/Dropbox/R Library/rSSOAP/DATA/lenox.RData")
load("C:/Users/Craig/Dropbox/R Library/rSSOAP/DATA/tidy_rain.RData")

library(tidyverse); theme_set(theme_bw())
library(lubridate)
library(stringr)
library(rSSOAP)
library(zoo)
library(vars)
library(imputeTS)

# convey hourly data to daily data
rain_ <- tidy_rain %>%
  filter(station=="SOWEST") %>%
  mutate(date=date(datetime)) %>%
  group_by(date) %>%
  summarize(rain=sum(rainfall_in, na.rm=TRUE))

rain_[is.na(rain_)]=0

tmp <- data %>%
  mutate(date=date(datetime)) %>%
  filter(date<mdy("01/01/2020")) %>%
  group_by(date) %>%
  summarize(df=sum(flow_gpm, na.rm=TRUE)) %>%
  left_join(rain_, by="date")

#tmp$rain[is.na(tmp$rain)]=0



date <- tmp$date
flow <- tmp$df
rain <- tmp$rain



# Daily Flow Object
df <- data.frame(date=date, flow=flow, rain=rain)

# Remove Outliers
MAX_DF <- unname(quantile(df$flow, probs=0.95, na.rm=TRUE))*1.5
MIN_DF <- unname(quantile(df$flow, probs=0.25, na.rm=TRUE))/1.5

df_adj <- df$flow
df_adj[df_adj>MAX_DF]=NA
df_adj[df_adj<MIN_DF]=NA
df_adj <- na_interpolation(df_adj, option="linear")

df$df_adj <- df_adj



# Dry-Weather Flow Parameters
MAX_RAIN_TODAY=0.25
MAX_RAIN_SHORT=0.5
DAY_RAIN_SHORT=7
MAX_RAIN_LONG=2
DAY_RAIN_LONG=14
MAX_STDEV=0.5

MAX_FLOW <- mean(flow)+MAX_STDEV*sd(flow)
MIN_FLOW <- mean(flow)-MAX_STDEV*sd(flow)

dwf <- df %>%
  mutate(lag_short=rollapply(rain, DAY_RAIN_SHORT, sum, fill=0)) %>%
  mutate(lag_long=rollapply(rain, DAY_RAIN_LONG, sum, fill=0)) %>%
  filter(rain<= MAX_RAIN_TODAY & lag_short<=MAX_RAIN_SHORT & lag_long <= MAX_RAIN_LONG) %>%
  filter(df_adj<=MAX_FLOW & df_adj >= MIN_FLOW) %>%
  mutate(wday=wday(date)) %>%
  group_by(wday) %>%
  summarize(dwf=mean(df_adj))

gwi <- df %>%
  mutate(wday=wday(date)) %>%
  left_join(dwf, by="wday") %>%
  mutate(dwf_adj=na_interpolation(dwf, option="linear")) %>%
  mutate(wwf=df_adj-dwf) %>%
  mutate(gwi=zoo::rollapply(wwf, 30, quantile, prob=0.05, fill=NA))



# MAX_GWI <- unname(quantile(gwi$gwi, probs=0.95, na.rm=TRUE))*1.5
# MIN_GWI <- unname(quantile(gwi$gwi, probs=0.25, na.rm=TRUE))*1.5
#
# gwi_adj <- gwi$gwi
# gwi_adj[gwi_adj>MAX_GWI]=NA
# gwi_adj[gwi_adj<MIN_GWI]=NA
# gwi_adj <- na_interpolation(gwi_adj, option="linear")

ZERO_GWI <- unname(quantile(gwi_adj, probs=0.05, na.rm=TRUE))
gwi_adj <- gwi_adj - ZERO_GWI

gwi$gwi_adj <- gwi_adj




bsf <- gwi %>%
  mutate(bsf=df_adj-gwi_adj)

MAX_BSF <- unname(quantile(bsf$bsf, probs=0.95, na.rm=TRUE))*1.5
MIN_BSF <- unname(quantile(bsf$bsf, probs=0.25, na.rm=TRUE))/1.5

bsf_adj <- bsf$bsf
bsf_adj[bsf_adj>MAX_BSF]=NA
bsf_adj[bsf_adj<MIN_BSF]=NA
bsf_adj <- na_interpolation(bsf_adj, option="linear")

bsf$bsf_adj <- bsf_adj

df <- bsf %>%
  dplyr::select(date, flow, rain, wday, df_adj, bsf_adj)#, dwf=dwf_adj, gwi=gwi_adj)

dwf <- df %>%
  mutate(lag_short=rollapply(rain, DAY_RAIN_SHORT, sum, fill=0)) %>%
  mutate(lag_long=rollapply(rain, DAY_RAIN_LONG, sum, fill=0)) %>%
  filter(rain<= MAX_RAIN_TODAY & lag_short<=MAX_RAIN_SHORT & lag_long <= MAX_RAIN_LONG) %>%
  filter(flow<=MAX_FLOW & flow >= MIN_FLOW) %>%
  group_by(wday) %>%
  summarize(dwf=mean(bsf_adj))

gwi <- df %>%
  #mutate(wday=wday(date)) %>%
  left_join(dwf, by="wday") %>%
  #mutate(dwf_adj=na_interpolation(dwf, option="linear")) %>%
  mutate(wwf=df_adj-dwf) %>%
  mutate(gwi=zoo::rollapply(wwf, 30, quantile, prob=0.05, fill=NA))

gwi_adj <- gwi$gwi
gwi_adj[gwi_adj<0]=0
gwi_adj <- na_interpolation(gwi_adj, option="linear")
gwi$gwi_adj <- gwi_adj

gwi %>%
  mutate(wwf=df_adj-dwf-gwi_adj)

date <- gwi$date
flow <- gwi$wwf-gwi$gwi_adj
rain <- gwi$rain
plot(gwi$wwf, type="l")

flow <- na_interpolation(flow)
#infer_daily_hydrograph <- function(date, flow, rain, IA=0.5) {
  # Get Wet-WEather Component
  #rdii <- infer_daily_rdii(date, flow, rain)

IA = 0.25
rdii <- flow
  # Instead of events, use Inital Abstraction
  r <- rain
  r[r<IA]=0

  # Translate to time series for VAR
  ii <- ts(cbind(r, rdii))

  # Estimate the model
  var.1 <- VAR(ii, 2, type = "none")

  # Calculate the IRF
  ir.1 <- irf(var.1, impulse = "r", response = "rdii", n.ahead = 20, ortho = FALSE)

  # Return upper limit
  uh <- ir.1$Upper$r



PU <- lag_rain(rain)
UH <- uh
U <- matrix(c(UH, rep(0,ncol(PU)-length(UH))), ncol=1)
Q.m <- PU%*%U

gwi$rdii <- Q.m

final <- gwi %>%
  mutate(model=dwf+gwi_adj+rdii)

plot(final$flow, type="l", ylim=c(0,2e6))
lines(final$model, col="red")

qqplot(final$flow, final$model)
abline(a=0,b=1)

error <- (final$model-final$flow)/final$flow
