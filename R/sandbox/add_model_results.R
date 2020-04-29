library(tidyverse); theme_set(theme_bw())
library(lubridate)
library(stringr)
library(rSSOAP)
library(zoo)
library(vars)

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

tmp$rain[is.na(tmp$rain)]=0

date <- tmp$date
flow <- tmp$df
rain <- tmp$rain

max.rain=0.1
max.rain.short=0.25
dry.days.short=7
max.rain.long=2
dry.days.long=14
max.stdev=0.5

dwf <- infer_daily_dwf(tmp$date, tmp$df, tmp$rain)
gwi <- infer_daily_gwi(tmp$date, tmp$df, tmp$rain)
uh <- infer_daily_hydrograph(tmp$date, tmp$df, tmp$rain)

a <- tmp %>%
  mutate(wday=wday(date)) %>%
  mutate(wend=wday==1|wday==7) %>%
  mutate(dwf=dwf$weekday)

a$dwf[a$wend]=dwf$weekend
gwi[gwi<0]=0
a$gwi <- gwi

rdii <- convolute_matrix(tmp$date, tmp$df, tmp$rain, uh)
a$rdii <- rdii

b <- a %>%
  mutate(model=dwf+gwi+rdii)

plot(a$df, type="l", lty=2)
lines(b$model, col="red")

c <- quantile(a$df, probs=1:99/100)
d <- quantile(b$model, probs=1:99/100)

qqplot(c, d)
abline(a=0, b=1)

draw_daily_summary(tmp$date, tmp$df, tmp$rain)

