load("C:/Users/Craig/Dropbox/R Library/rSSOAP/DATA/tidy_rain.RData")
load("C:/Users/Craig/Dropbox/R Library/WWSP/data/hrt.rda")

library(tidyverse); theme_set(theme_bw())
library(lubridate)
library(stringr)
library(rSSOAP)
library(zoo)
library(vars)

# remove gwi from decomp


ps <- hrt %>%
  filter(grid=="west") %>%
  filter(str_detect(tolower(address), "duch*")) %>%
  mutate(date=date(datetime)) %>%
  filter(date>=mdy("01/01/2019")) %>%
  mutate(flow=runtime*500)

df <- ps %>%
  group_by(date) %>%
  summarize(df=sum(flow))

hourly_rain <- tidy_rain %>%
  filter(station=="SOWEST")

rain <- hourly_rain %>%
  mutate(date=date(datetime)) %>%
  group_by(date) %>%
  summarize(rain=sum(rainfall_in))

dry_event <- df %>%
  left_join(rain, by=c("date")) %>%
  mutate(lag_short=rollapply(rain, 7, sum, fill=0)) %>%
  mutate(lag_long=rollapply(rain, 14, sum, fill=0)) %>%
  filter(rain<= 0.25 & lag_short<=0.5 & lag_long <= 2)

dwf <- ps %>%
  inner_join(dry_event, by=c("date")) %>%
  mutate(wday=wday(datetime)) %>%
  mutate(hour=hour(datetime)) %>%
  mutate(wdayhour=wday+hour/24) %>%
  group_by(wdayhour) %>%
  summarize(dwf=mean(flow))

wwf <- ps %>%
  mutate(wday=wday(datetime)) %>%
  mutate(hour=hour(datetime)) %>%
  mutate(wdayhour=wday+hour/24) %>%
  left_join(dwf, by=c("wdayhour")) %>%
  mutate(wwf=flow-dwf) %>%
  left_join(hourly_rain, by=c("datetime"))

gwi <- wwf %>%
  group_by(date) %>%
  summarize(df_wwf=mean(wwf)) %>%
  mutate(rp=zoo::rollapply(df_wwf, 30, quantile, prob=0.05, fill=0))

adj <- ps %>%
  left_join(gwi, by=c("date")) %>%
  mutate(adj=flow-rp)

wwf <- adj %>%
  mutate(wday=wday(datetime)) %>%
  mutate(hour=hour(datetime)) %>%
  mutate(wdayhour=wday+hour/24) %>%
  left_join(dwf, by=c("wdayhour")) %>%
  mutate(wwf=flow-dwf) %>%
  left_join(hourly_rain, by=c("datetime"))


use <- wwf %>%
  ungroup() %>%
  mutate(wwf_gpm=wwf/60) %>%
  dplyr::select(wwf=wwf_gpm, rainfall_in)


use[is.na(use$rainfall_in), ]=0

v <- VAR(use, 2, type="none")
i <- irf(v, impulse="rainfall_in", response="wwf", n.ahead=24*2, ortho=FALSE)
plot(i$Upper$rainfall_in, type="l")

wwf %>%
  ggplot(aes(datetime, wwf/60)) +
  geom_line() +
  geom_step(aes(y=rainfall_in*100), col="blue", lwd=1)


wwf %>%
  mutate(flo=dwf+rp) %>%
  ggplot(aes(x=datetime, y=flo)) +
  geom_line()
