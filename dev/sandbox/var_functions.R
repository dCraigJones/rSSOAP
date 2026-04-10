date <- DF$date
flow <- DF$Hollybrook
rain <- DF$rain
IA <- 0.5

Get.Hydrograph.var <- function(date, flow, rain, IA=0.5) {
    # Get Wet-WEather Component
    rdii <- Get.RDII(date, flow, rain)

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


date <- DF$date
flow <- DF$Hollybrook
rain <- DF$rain
hydrograph <- uh


convolute_error <- function(date, flow, rain, hydrograph) {
  rdii <- Get.RDII(date, flow, rain)

  PU <- Lag.Rain(rain)
  UH <- hydrograph
  U <- matrix(c(UH, rep(0,ncol(PU)-length(UH))), ncol=1)
  Q.m <- PU%*%U

  residual <- rdii-Q.m

  error <- mean(residual)/mean(flow)

  return(error)
}


ia <- seq(0,1,0.1)
error <- NULL

for (i in seq_along(ia)) {
  uh <- Get.Hydrograph.var(DF$date, DF$Laura, DF$rain, i)
  error[i] <- convolute_error(DF$date, DF$Laura, DF$rain, uh)
}
