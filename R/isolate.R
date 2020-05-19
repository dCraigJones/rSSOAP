isolate_daily_dwf <- function(date, flow, rain
                            , MAX_RAIN_TODAY=0.25
                            , MAX_RAIN_SHORT=0.5
                            , DAY_RAIN_SHORT=7
                            , MAX_RAIN_LONG=2
                            , DAY_RAIN_LONG=14
                            , MAX_STDEV=1) {

  MAX_FLOW <- mean(flow)+MAX_STDEV*sd(flow)
  MIN_FLOW <- mean(flow)-MAX_STDEV*sd(flow)

  tmp <- data.frame(date=date, flow=flow, rain=rain)

  dwf <- tmp %>%
    mutate(lag_short=zoo::rollapply(rain, DAY_RAIN_SHORT, sum, align="right", fill=0)) %>%
    mutate(lag_long=zoo::rollapply(rain, DAY_RAIN_LONG, sum, align="right", fill=0)) %>%
    filter(rain<= MAX_RAIN_TODAY & lag_short<=MAX_RAIN_SHORT & lag_long <= MAX_RAIN_LONG) %>%
    filter(flow<=MAX_FLOW & flow >= MIN_FLOW) %>%
    mutate(wday=wday(date)) %>%
    group_by(wday) %>%
    summarize(dwf=mean(flow))

  return(dwf)
}

isolate_daily_gwi <- function(date, flow, rain, HIGH_PASS=0.1) {
  dwf <- isolate_daily_dwf(date, flow, rain)

  tmp <- data.frame(date=date, flow=flow, rain=rain)

  gwi <- tmp %>%
    mutate(wday=lubridate::wday(date)) %>%
    left_join(dwf, by="wday") %>%
    #mutate(dwf_adj=imputeTS::na_interpolation(dwf, option="linear")) %>%
    mutate(wwf=flow-dwf) %>%
    mutate(gwi=zoo::rollapply(wwf, 30, align="right", quantile, prob=HIGH_PASS, fill=NA))

  retVal <- remove_outliers(zero_offset(gwi$gwi))

  return(unname(retVal))
}

isolate_daily_bsf <- function(date, flow, rain) {
  #gwi <- isolate_daily_gwi(date, flow, rain)

  tmp <- data.frame(date=date, flow=flow, rain=rain)

  diurnal <- isolate_daily_dwf(date, flow, rain)


  retVal <- tmp %>%
    mutate(wday=lubridate::wday(date)) %>%
    left_join(diurnal, by=c("wday"))

  return(unname(retVal$dwf))
}



isolate_daily_rdi <- function(date, flow, rain, INITIAL_ABSTRACTION=0.5) {

  uh <- isolate_daily_hydrograph(date, flow, rain, INITIAL_ABSTRACTION)

  PU <- lag_rain(rain)
  UH <- uh
  U <- matrix(c(UH, rep(0,ncol(PU)-length(UH))), ncol=1)
  Q.m <- PU%*%U

  return(Q.m)
}
