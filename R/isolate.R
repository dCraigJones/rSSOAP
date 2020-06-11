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


isolate_hourly_dwf <- function(date, flow, rain
                               , MAX_RAIN_CURRENT=0.1
                               , MAX_RAIN_SHORT=0.5
                               , HOUR_RAIN_SHORT=12
                               , MAX_RAIN_LONG=1
                               , HOUR_RAIN_LONG=24
                               , MAX_STDEV=2) {

  # Pump flow can have spikes due to non-rainfall related causes
  # abnormal values are filters based on number of stnd deviations.
  MAX_FLOW <- mean(flow, na.rm=TRUE)+MAX_STDEV*sd(flow, na.rm=TRUE)
  MIN_FLOW <- mean(flow, na.rm=TRUE)-MAX_STDEV*sd(flow, na.rm=TRUE)

  tmp <- data.frame(date=date, flow=flow, rain=rain)

  dwf <- tmp %>%
    # lag_short is sum of previous DAY_HOUR_SHORT hours of rain
    mutate(lag_short=zoo::rollapply(rain, HOUR_RAIN_SHORT, sum, align="right", fill=0)) %>%
    # lag_long is sum of previous HOUR_RAIN_LONG hours of rain
    mutate(lag_long=zoo::rollapply(rain, HOUR_RAIN_LONG, sum, align="right", fill=0)) %>%
    # Identify dry-period by filtering for current and totalized rainfall
    filter(rain<= MAX_RAIN_CURRENT & lag_short<=MAX_RAIN_SHORT & lag_long <= MAX_RAIN_LONG) %>%
    # remove abnormal values that exceed stnd deviation criteria
    filter(flow<=MAX_FLOW & flow >= MIN_FLOW) %>%
    # convert to decimal days (Mon 6AM = 2.25) for diurnal calculation
    mutate(wday=wday(date)+hour(date)/24) %>%
    # calculate dirunal as mean of flow by decimal day
    group_by(wday) %>%
    summarize(dwf=mean(flow, na.rm=TRUE))

  return(dwf)
}


isolate_hourly_gwi <- function(datetime, flow, rain, pct_of_ntf=0.8){
  tmp <- data.frame(datetime, flow, rain)

  ntf <- tmp %>%
    mutate(date=date(datetime)) %>%
    mutate(hour=hour(datetime)) %>%
    filter(hour>20 | hour<6) %>%
    group_by(date) %>%
    summarize(ntf=min(flow))

  ntf$ntf2 <- remove_outliers(ntf$ntf)

  use <- tmp %>%
    mutate(date=date(datetime)) %>%
    left_join(ntf, by="date") %>%
    mutate(gwi=ntf2*pct_of_ntf) %>%
    dplyr::select(datetime, flow, rain, gwi) %>%
    mutate(wk=week(date))

  export <- use %>%
    group_by(wk) %>%
    summarize(gwi2=mean(gwi, na.rm=TRUE)) %>%
    right_join(use, by="wk")

  #return(export$gwi2)
  return(use$gwi)

}

isolate_hourly_bsf <- function(datetime, flow, rain, diurnal) {
  tmp <- data.frame(datetime, flow, rain)

  export <- tmp %>%
    mutate(wday=wday(datetime)+hour(datetime)/24) %>%
    left_join(diurnal, by="wday") %>%
    mutate(bsf=dwf)

  return(export$bsf)
}
