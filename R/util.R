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
