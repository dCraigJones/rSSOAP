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
#' Get.DWF(DF$date, DF$Buffalo, DF$rain)
Get.DWF <- function(date,flow,rain,max.rain.short=0, dry.days.short=7, max.rain.long=1, dry.days.long=14, max.stdev=0.5) {
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
#' Get.DWF(DF$date, DF$Buffalo, DF$rain)
Get.WWF <- function(date,flow,rain) {
  DWF <- Get.DWF(date,flow,rain)
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
#' Get.GWI(DF$date, DF$Buffalo, DF$rain)
Get.GWI <- function(date, flow, rain) {

  delta <- Get.WWF(date, flow, rain)

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
#' Get.RDII(DF$date, DF$Buffalo, DF$rain)
Get.RDII <- function(date, flow, rain) {
  flow <- Get.WWF(date, flow, rain) - Get.GWI(date, flow, rain)

  return(flow)

}

#' Identify major RDII events
#'
#' @param date vector of date for flow and rain data
#' @param flow vector of flow data, in GPD
#' @param rain vector of rain, in inches
#' @param min.rain Minimum Rainfall, inches, over the preceeding number of _wet.days_ to register a wet event
#' @param wet.days Preceeding number of days for event to register given _min.rain_ is exceeded
#' @param min.event Minimum Rainfall, inches, for the day to register as an event
#' @param min.sd Minimum number of standard deviations flow must exceed to register
#' @param max.sd Maximum number of standard deviations flow can have to register
#'
#' @return vector of significant, single day events
#' @export
#'
#' @examples
#' data(DF)
#' Get.Events(DF$date, DF$Buffalo, DF$rain)
Get.Events <- function(date,flow,rain, min.rain=3, wet.days=7,min.event=1, min.sd=2, max.sd=99) {

  flow <- Get.RDII(date, flow, rain)

  WWF.amc <- filter(rain, rep(1/wet.days,wet.days))*wet.days>=min.rain
  WWF.event <- rain>=min.event
  WWF.sd.min <- flow>(mean(flow)+min.sd*sd(flow))
  WWF.sd.max <- flow<(mean(flow)+max.sd*sd(flow))

  WWF.use <- WWF.amc & WWF.event & WWF.sd.min & WWF.sd.max

  return(unname(which(WWF.use)))

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
#' Lag.Rain(DF$rain)
Lag.Rain <- function(P) {
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

#' Cost Function for GA optimizer
#'
#' @param Q Daily flow, in GPD
#' @param PU Square rainfall matrix
#' @param DateRange Time Window for analysis
#' @param x1 Day 01
#' @param x2 Day 02
#' @param x3 Day 03
#' @param x4 ...
#' @param x5 ...
#' @param x6 ...
#' @param x7 ...
#' @param x8 ...
#' @param x9 ...
#' @param x10 ...
#' @param x11 ...
#' @param x12 ...
#' @param x13 Day 13
#' @param x14 Day 14
#' @param x15 Day 15
#'
#' @return Mean-Squared Error between actual flow and hydrograph flow
Get.Cost <- function(Q, PU, DateRange, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15) {
  UH <- c(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15)
  U <- matrix(c(UH, rep(0,ncol(PU)-length(UH))), ncol=1)

  Q.m <- PU%*%U
  MSE <- sum((Q[DateRange]-Q.m[DateRange])^2)

  return(MSE)
}

#' Get Hydrograph
#'
#' @param Q Daily flow, in GPD
#' @param PU Square rainfall matrix
#' @param DateRange Time Window for analysis
#'
#' @return 15-day Unit Hydrograph
#' @export
#'
#' @examples
#' data(DF)
#' Ev <- Get.Events(DF$date, DF$Barnes, DF$rain)
#' RD <- Get.RDII(DF$date, DF$Barnes, DF$rain)
#' PU <- Lag.Rain(DF$rain)
#' \dontrun{Get.Hydrograph(RD, PU, -21:21+Ev[1])}
Get.Hydrograph <- function(Q,PU,DateRange) {
  max.flow <- max(Q)

  A <- GA::ga(type="real-valued"
          , fitness = function(x) - Get.Cost(Q,PU,DateRange,x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8], x[9], x[10], x[11], x[12], x[13], x[14], x[15])
          , lower = rep(0,15)
          , upper = c(rep(max.flow,5),rep(max.flow/2,5),rep(max.flow/4,5))
          , popSize=100
          , maxiter = 1000
  )

  U.m <- as.matrix(A@solution)

  retVal <- U.m[1:15]

  return(retVal)
}

