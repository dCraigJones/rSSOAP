shape_daily_hydrograph <- function(date, signal, rain, MODEL_DURATION=20, INITIAL_ABSTRACTION = 0.5) {

  rdi <- signal

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
  uh <- ir.1$Upper$r

  return(uh)
}
