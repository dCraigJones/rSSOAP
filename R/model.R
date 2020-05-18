model_hydrograph <- function(rain, uh, INITIAL_ABSTRACTION=0) {
  r <- rain
  r[is.na(r)]=0
  r[r<INITIAL_ABSTRACTION]=0

  PU <- lag_rain(rain)
  UH <- uh
  U <- matrix(c(UH, rep(0,ncol(PU)-length(UH))), ncol=1)
  Q.m <- PU%*%U

  return(Q.m)
}
