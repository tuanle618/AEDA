#' @title Computes a suitable eps value for DBScan
#'
#' @description
#'  The criterion used is an elbow criterion for knn distancies.
#'  Since this is a subjective criterion the calculation is just heuristic
#'
#' @param data [\code{data.frame}]\cr
#'   A Dataframe with different numeric variables.
#' @return [\code{numeric(1)}]
#'   An eps value for dbscan
#'
#' @example
#'   getEps(iris[,1:4])
#' @import checkmate
#' @importFrom pracma gradient
getEps = function(data) {
  dists = kNNdist(data, k = 5)
  y = sort(a)
  x = seq(1, length(y))
  f = gradient(y, x)
  # make "later" values bigger by weighting with the decreasing knnDists
  # add mean againt dividing by 0
  wf = f / (sort(a, decreasing = TRUE) + mean(a))
  # remove small values
  big.wf = wf[wf > mean(wf)]
  big.x3 = x[wf > mean(wf)]
  trim.wf = big.wf[big.wf <= quantile(big.wf, 0.85)]
  trim.x = big.x3[big.wf <= quantile(big.wf, 0.85)]
  eps.ind = trim.x[trim.wf == max(trim.wf)]
  # in case multiple max are found
  mean(y[eps.ind])
}
