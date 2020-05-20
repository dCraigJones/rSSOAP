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
