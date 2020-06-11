shape_daily_hydrograph <- function(date, signal, rain, MODEL_DURATION=20, INITIAL_ABSTRACTION = 0.5, SCALE=1) {

  rdi <- signal
  rdi <- zero_offset(rdi)
  rdi[rdi<0]=0

  r <- rain
  r[r<INITIAL_ABSTRACTION]=0
  r[is.na(r)]=0

  # Translate to time series for VAR
  ii <- ts(cbind(r, rdi))
  colnames(ii) <- c("r", "rdi")

  # Estimate the model
  var.1 <- VAR(ii, 2, type = "none")

  # Calculate the IRF
  ir.1 <- irf(var.1, impulse = "r", response = "rdi", n.ahead = MODEL_DURATION, ortho = FALSE)

  # Return upper limit
  #uh <- (ir.1$Upper$r+ir.1$irf$r)/2
  uh <- ir.1$Upper$r*SCALE

  uh[uh<0]=0

  return(uh)
}


shape_gwi_hydrograph <- function(date, signal, rain, MODEL_DURATION=60, INITIAL_ABSTRACTION = 0.25) {
    rdi <- df$gwi

    r <- rain
    r[is.na(r)]=0
    r[r<INITIAL_ABSTRACTION]=0

    r <- rollapply(r, 3, mean, fill=0)

    # Translate to time series for VAR
    ii <- ts(cbind(r, rdi))
    colnames(ii) <- c("r", "rdi")

    # Estimate the model
    var.1 <- VAR(ii, 2, type = "none")

    # Calculate the IRF
    ir.1 <- irf(var.1, impulse = "r", response = "rdi", n.ahead = MODEL_DURATION, ortho = FALSE)

    # Return upper limit
    #uh <- (ir.1$Upper$r+ir.1$irf$r)/2
    uh <- ir.1$Upper$r

    return(uh)
}


shape_hourly_hydrograph <- function(date, signal, rain, MODEL_DURATION=20, INITIAL_ABSTRACTION = 0.5, SCALE=1) {

  # date <- hf$datetime
  # signal <- hf$flow-hf$gwi-hf$bsf
  # rain <- hf$rain
  #
  # MODEL_DURATION=24
  # INITIAL_ABSTRACTION = 0
  # SCALE=1

  rdi <- signal
  #rdi <- zero_offset(rdi)
  rdi[rdi<0]=0
  rdi[is.na(rdi)]=0

  r <- rain
  r[r<INITIAL_ABSTRACTION]=0
  r[is.na(r)]=0

  # Translate to time series for VAR
  ii <- ts(cbind(r, rdi))
  colnames(ii) <- c("r", "rdi")

  # Estimate the model
  var.1 <- VAR(ii, 2, type = "none")

  # Calculate the IRF
  ir.1 <- irf(var.1, impulse = "r", response = "rdi", n.ahead = MODEL_DURATION, ortho = FALSE)

  # Return upper limit
  #uh <- (ir.1$Upper$r+ir.1$irf$r)/2
  uh <- ir.1$Upper$r*SCALE

  uh[uh<0]=0

  return(uh)
}


shape_hourly_diurnal <- function(date, flow, rain
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

  diurnal <- tmp %>%
    # lag_short is sum of previous DAY_HOUR_SHORT hours of rain
    mutate(lag_short=zoo::rollapply(rain, DAY_RAIN_SHORT, sum, align="right", fill=0)) %>%
    # lag_long is sum of previous HOUR_RAIN_LONG hours of rain
    mutate(lag_long=zoo::rollapply(rain, DAY_RAIN_LONG, sum, align="right", fill=0)) %>%
    # Identify dry-period by filtering for current and totalized rainfall
    filter(rain<= MAX_RAIN_CURRENT & lag_short<=MAX_RAIN_SHORT & lag_long <= MAX_RAIN_LONG) %>%
    # remove abnormal values that exceed stnd deviation criteria
    filter(flow<=MAX_FLOW & flow >= MIN_FLOW) %>%
    # convert to decimal days (Mon 6AM = 2.25) for diurnal calculation
    mutate(wday=wday(date)+hour(date)/24) %>%
    # calculate dirunal as mean of flow by decimal day
    group_by(wday) %>%
    summarize(diurnal=mean(flow, na.rm=TRUE))

  return(diurnal)
}
