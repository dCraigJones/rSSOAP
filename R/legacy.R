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

#' Summarize Wet-Weather Flow
#'
#' @param date vector of date for flow and rain data
#' @param flow vector of flow data, in GPD
#' @param rain vector of rain, in inches
#' @param H unit hydrograph from output (optional)
#'
#' @return list
#' @export
#'
#' @examples
#' data(DF)
#' \dontrun{Get.Summary(DF$date, DF$Hollybrook, DF$rain)}
Get.Summary <- function(date, flow, rain, H=NA) {
  DWF <- Get.DWF(date, flow, rain)

  tmp <- Get.GWI(date, flow, rain)
  tmp <- tmp[tmp>0]
  GWI <- quantile(tmp, probs=c(0.25, 0.50, 0.75, 0.90, 0.95, 0.99))

  Ev <- Get.Events(date, flow, rain)

  if(length(Ev) < 2)
    stop("Not enough wet-weather flow variation for analysis")

  RD <- Get.RDII(date, flow, rain)

  PU <- Lag.Rain(rain)

  if(anyNA(H)) {
    H <- matrix(c(rep(0,15*length(Ev))), ncol=15)

    for (i in 1:nrow(H)) {
      H[i,] <- Get.Hydrograph(RD, PU, -21:21+Ev[i])
    }
  }

  R <- rain[Ev]
  I <- apply(H,1,max)*R
  mean_Rt <- apply(H,1,mean)
  fit <- lm(I/1e3~R)
  r2 <- round(cor(R,I),2)
  Rt <- round(coef(fit)[2],2)*1e3
  y <- round(coef(fit)[1],2)*1e3
  IA <- -y/Rt

  RDII <- c(IA, Rt, r2, mean_Rt)
  names(RDII) <- c("Initial Abstraction (in)", "Total Runoff (GPD/inch)", "r2", "Mean Runoff (GPD/inch)")

  model <- list("DWF (GPD)"=DWF, "GWI (GPD)"=GWI, "RDII (GPD/Inch)"=RDII, "UH (GPD/Inch)"=H)

  return(model)
}

#' Draw 4-panel graph
#'
#' @param date vector of date for flow and rain data
#' @param flow vector of flow data, in GPD
#' @param rain vector of rain, in inches
#' @param H unit hydrograph from output (optional)
#'
#' @return unit hydrograph for each event
#' @export
#'
#' @examples
#' data(DF)
#' \dontrun{Draw.Panels(DF$date, DF$Hollybrook, DF$rain)}
Draw.Panels <- function(date, flow, rain, H=NA) {
  Max.Daily.Flow = ceiling(max(flow)/1e6)

  #Max.RDII = 1500

  Ev <- Get.Events(date, flow, rain)

  if(length(Ev) < 2)
    stop("Not enough wet-weather flow variation for analysis")

  RD <- Get.RDII(date, flow, rain)

  PU <- Lag.Rain(rain)

  if(anyNA(H)) {
    H <- matrix(c(rep(0,15*length(Ev))), ncol=15)

    for (i in 1:nrow(H)) {
      H[i,] <- Get.Hydrograph(RD, PU, -21:21+Ev[i])
    }
  }



  layout(matrix(c(1,2,3,4,4,4), ncol=2, byrow = FALSE), widths=c(1,3))


  DWF <- Get.DWF(date, flow, rain)
  barplot(c(DWF$weekend, DWF$weekday)/1e3
          , main="DWF"
          , names.arg=c("wkend", "wkday")
          #, names.arg=paste(prettyNum(round(c(DWF$weekday, DWF$weekend)/1e3,0), big.mark = ","), "kGPD")
          , col=c("grey90", "white")
          , horiz=TRUE
          , xlab="Daily Flow (kGPD)"
  )
  box(bty="l")

  GWI <- Get.GWI(date, flow, rain)
  GWI <- GWI[GWI>0]
  plot(density(GWI/1e3)
       , main="GWI"
       , xlab="kGPD"
       , lwd=2
       , bty="l"
  )

  R <- rain[Ev]
  I <- apply(H,1,max)*R
  fit <- lm(I/1e3~R)
  r2 <- round(cor(R,I),2)
  Rt <- round(coef(fit)[2],2)

  plot(R,I/1e3
       , main="RDII"
       , xlim=c(0,6)
       , ylim=c(0,Rt*6)
       , cex=1.5
       , pch=4
       , lwd=2
       , ylab="kGPD/inch"
       , xlab="Rain (inch)"
       , bty="l"
  )
  abline(fit, lty=2)

  par(mar=c(6,5,3,5)) #Btm, Left, Top, Right
  plot(date, flow/1e6
       , lwd=2
       , type="l"
       , xaxs="i"
       , yaxs="i"
       , xlab=NA
       , ylab="Daily Flow (MGD)"
       , axes=FALSE
       , ylim=c(0,Max.Daily.Flow)
  )

  lines(date, rain*Max.Daily.Flow/10, type="h", col="blue")

  # Axis Label - Month/Year
  Month <- c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")

  dd <- date[which(lubridate::day(date)==1)]
  dl <- Month[lubridate::month(dd)]

  axis(1, at=dd, labels=dl)

  yd <- which(lubridate::yday(date)==1)
  yl <- lubridate::year(date[yd])

  axis(1, at=date[yd], labels=yl, line=2, lwd=0)

  # Axis Labels - Daily Flow/Rain
  axis(2)
  axis(4, at=seq(0,Max.Daily.Flow,length.out=6), labels=seq(0,10,2))

  mtext("Rain (inches)", side=4, line=3, cex=0.7)


  box(lwd=1)

  # Reference Lines
  mDWF <- (DWF$weekday+DWF$weekend)/2
  MA <- unname(max(DWF)+quantile(GWI,0.9)+Rt*5*1e3)
  Jax5YR <- unname(max(DWF)+quantile(GWI,0.9)+Rt*6.5*1e3)
  Jax25YR <- unname(max(DWF)+quantile(GWI,0.9)+Rt*9.5*1e3)
  q75th <- as.numeric(quantile(rain[rain>0.5],0.75)*Rt+DWF[1]/1e3+quantile(GWI,0.75)/1e3)*1e3
  abline(h=c(mDWF, q75th, MA, Jax5YR, Jax25YR)/1e6, lty=2)
  axis(2, at=c(mDWF, q75th, MA, Jax5YR, Jax25YR)/1e6, labels=c("DWF", "3Q", "2YR", "5YR", "25YR"), line=1, lwd=0)
  axis(4, at=c(mDWF, q75th, MA, Jax5YR, Jax25YR)/1e6, labels=c("DWF", "3Q", "2YR", "5YR", "25YR"), line=1, lwd=0)

  legend("top"
         , c("Flow", "Rain")
         , lwd=c(2,2)
         , lty=c(1,1)
         , col=c("black", "blue")
         , inset=c(0.05,0.05)
         , seg.len = 4
         , pch=c(NA,NA)
         , cex=1.0
         , y.intersp=1.0
         , bty = "n"
         , box.col=rgb(1,1,1,0.75)
         , bg=rgb(1,1,1,0.75)
         , horiz=FALSE
         , text.font=2
  )

  Get.Summary(date, flow, rain, H)

  return(H)
}
