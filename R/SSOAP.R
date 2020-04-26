#' Calculates Dry-Weather Flow (DWF)
#'
#' @param date vector of date for flow and rain data
#' @param flow vector of flow data, in GPD
#' @param rain vector of rain, in inches
#' @param max.rain.short maximum allowable rain during the short-term
#' @param dry.days.short number of days considered in the short-term
#' @param max.rain.long  maximum allowable rain during the long-term
#' @param dry.days.long  number of days considered in the long-term
#' @param max.stdev maximum allowable standard deviations in any term
#'
#' @return a dataframe of DWF by weekend and weekday
#' @export
#'
#' @examples
#' data(DF)
#' infer_daily_dwf(DF$date, DF$Buffalo, DF$rain)
infer_daily_dwf <- function(date,flow,rain,max.rain.short=0, dry.days.short=7, max.rain.long=1, dry.days.long=14, max.stdev=0.5) {
  DWF.amc.short <- !filter(rain, rep(1/dry.days.short,dry.days.short))>max.rain.short
  DWF.amc.short[is.na(DWF.amc.short)] <- FALSE

  DWF.amc.long <- !filter(rain, rep(1/dry.days.long,dry.days.long))>max.rain.long
  DWF.amc.long[is.na(DWF.amc.long)] <- FALSE

  is.wkd <- timeDate::isWeekday(date)
  #is.hol <- isHoliday(as.timeDate(df$date))

  #use <- df[DWF.amc.short & is.wkd & DWF.amc.long & !is.hol,]
  use <- flow[DWF.amc.short & is.wkd & DWF.amc.long]

  RDS <- use < (mean(use)+max.stdev*sd(use))

  DWF.wkd <- mean(use[RDS])

  #use <- df[DWF.amc.short & !is.wkd & DWF.amc.long & !is.hol,]
  use <- flow[DWF.amc.short & !is.wkd & DWF.amc.long]

  RDS <- use < (mean(use)+max.stdev*sd(use))

  DWF.wke <- mean(use[RDS])

  DWF <- data.frame(weekday=DWF.wkd, weekend=DWF.wke)

  return(DWF)
}

#' Calculate Wet-Weather Flow (WWF).
#'
#' @param date vector of date for flow and rain data
#' @param flow vector of flow data, in GPD
#' @param rain vector of rain, in inches
#'
#' @return vector of flow minus DWF
#' @export
#'
#' @examples
#' data(DF)
#' infer_daily_wwf(DF$date, DF$Buffalo, DF$rain)
infer_daily_wwf <- function(date,flow,rain) {
  DWF <- infer_daily_dwf(date,flow,rain)
  is.wkd <- timeDate::isWeekday(date)

  . <- flow
  .[is.wkd] <- flow[is.wkd]-DWF$weekday
  .[!is.wkd] <- flow[!is.wkd]-DWF$weekend

  return(.)

}

#' Calculate Groundwater Intrusion (GWI)
#'
#' @param date vector of date for flow and rain data
#' @param flow vector of flow data, in GPD
#' @param rain vector of rain, in inches
#'
#' @return vector of flow minus DWF
#' @export
#'
#' @examples
#' data(DF)
#' infer_daily_gwi(DF$date, DF$Buffalo, DF$rain)
infer_daily_gwi <- function(date, flow, rain) {

  delta <- infer_daily_wwf(date, flow, rain)

  GWI <- zoo::rollapply(delta, 29, quantile, prob=0.05)
  GWI <- c(rep(0,14), GWI, rep(0,14))

  return(unname(GWI))
}


#' Get Rainfall-Derived Inflow and Infiltration (RDII)
#'
#' @param date vector of date for flow and rain data
#' @param flow vector of flow data, in GPD
#' @param rain vector of rain, in inches
#'
#' @return vector of flow minus DWF minus GWI
#' @export
#'
#' @examples
#' data(DF)
#' infer_daily_rdii(DF$date, DF$Buffalo, DF$rain)
infer_daily_rdii <- function(date, flow, rain) {
  flow <- infer_daily_wwf(date, flow, rain) - infer_daily_gwi(date, flow, rain)

  return(flow)

}





#' Get rainfall matrix
#'
#' `P <- lagmatrix(R)` creates a lagged (shifted) version of a vector.  The _lagmatrix_ function is useful for creating a regression matrix of explanatory variables for rainfall over time.
#'
#' Given a vector of time series data, such as daily rainfall:
#'   $$ \left[ P_1, P_2, P_3, \cdots , P_m \right]$$
#'   A lagged transform of the time series is found by shifting the first lag, then the second lag, and so on, until the end of the vector.  The _lagmatrix_ result will be the following:
#'
#'   $$
#'   \begin{bmatrix}
#' P_1 & 0 & 0 & \cdots & 0 & 0 & \cdots & 0 & 0  \\
#' P_2 & P_1 & 0 & \cdots & 0 & 0 & \cdots & 0 & 0  \\
#' P_3 & P_2 & P_1 & \cdots & 0 & 0 & \cdots & 0 & 0  \\
#' \vdots & &&& \vdots &&& \vdots \\
#' P_m & P_{m-1} & P_{m-2} & \cdots & P_1 & 0 & \cdots & 0 & 0  \\
#' 0 & P_{m} & P_{m-1} & \cdots & P_2 & P_1 & \cdots & 0 & 0  \\
#' \vdots & &&& \vdots &&& \vdots \\
#' 0 & 0 & 0 & \cdots & 0 & 0 & \cdots & P_m & P_{m-1}  \\
#' 0 & 0 & 0 & \cdots & 0 & 0 & \cdots & 0 & P_m  \\
#' \end{bmatrix}
#' $$
#'
#' @param P Daily Rainfall, in inches
#'
#' @return A square matrix of lagged rainfall hyetographs
#' @export
#'
#' @examples
#' data(DF)
#' lag_rain(DF$rain)
lag_rain <- function(P) {
  # Remove NA
  P[is.na(P)] <- 0

  # Create NULL PU Matrix
  PU <- matrix(rep(0,length(P)^2), ncol=length(P))

  # Build Matrix, PU
  Build.P.Matrix <- function(i) {
    i <- i - 1
    a <- 1:(length(P)-i)

    PU[a+i,i+1] <<- P[a]
  }

  . <- sapply(1:length(P), function(x) Build.P.Matrix(x))

  return(PU)
}

infer_daily_hydrograph <- function(date, flow, rain, IA=0.5) {
  # Get Wet-WEather Component
  rdii <- infer_daily_rdii(date, flow, rain)

  # Instead of events, use Inital Abstraction
  r <- DF$rain
  r[r<IA]=0

  # Translate to time series for VAR
  ii <- ts(cbind(rain, rdii))

  # Estimate the model
  var.1 <- VAR(ii, 2, type = "none")

  # Calculate the IRF
  ir.1 <- irf(var.1, impulse = "rain", response = "rdii", n.ahead = 20, ortho = FALSE)

  # Return upper limit
  uh <- ir.1$Upper$rain

  return(uh)
}

convolute_matrix <- function(date, flow, rain, hydrograph) {
  rdii <- infer_daily_rdii(date, flow, rain)

  PU <- lag_rain(rain)
  UH <- hydrograph
  U <- matrix(c(UH, rep(0,ncol(PU)-length(UH))), ncol=1)
  Q.m <- PU%*%U

  return(Q.m)
}

mpe_daily <- function(date, flow, rain, hydrograph) {
  field <- infer_daily_rdii(date, flow, rain)
  model <- convolute_matrix(date, flow, rain, hydrograph)

  residual <- field-model

  error <- mean(residual)/mean(flow)

  return(error)
}
