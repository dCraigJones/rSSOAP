#' Generates a Triangular Unit Hydrograph.
#'
#' @param r Rainfall Volume, in percent
#' @param t Time to Peak, in days
#' @param k Recession Constant
#'
#' @return
#' @export
#'
#' @examples
RTK <- function(r,t,k) {
  UH <- NULL
  maxLength <- 30

  h <- 2*r/t

  m.up <- h/t
  m.dn <- -h/((t*k))

  for (i in 1:maxLength)
    if (i>t) {
      UH[i] <- m.dn*(i-t)+h
    } else {
      UH[i] <- m.up*i
    }

  UH[UH<0] <- 0

  return(UH)
}
