remove_outliers <- function(rawVal
      , LOW_PASS = 0.99
      , HIGH_PASS = 0.10
      , SCALE = 1.5
    ) {

  q50 <- unname(quantile(rawVal, probs=0.50, na.rm=TRUE))
  qmax <- unname(quantile(rawVal, probs=LOW_PASS, na.rm=TRUE))
  qmin <- unname(quantile(rawVal, probs=HIGH_PASS, na.rm=TRUE))

  diff_min <- abs(ifelse (
        sign(qmin)==1
      , q50-qmin
      , qmin-q50))

  diff_max <- qmax-q50

  MAX_DF <- q50 + diff_max*SCALE
  MIN_DF <- q50 - diff_min*SCALE

  retVal <- rawVal
  retVal[retVal>MAX_DF]=NA
  retVal[retVal<MIN_DF]=NA
  retVal <- imputeTS::na_interpolation(retVal, option="linear")

  return(retVal)
}

zero_offset <- function(rawVal, HIGH_PASS=0.05) {
  ZERO_VALUE <- unname(quantile(rawVal, probs=HIGH_PASS, na.rm=TRUE))
  retVal <- rawVal - ZERO_VALUE

  retVal[retVal<0]=0

  return(retVal)
}
