#' @title Computes the factor analysis
#'
#' @description
#'  getFA computes the factor analysis of a numeric dataset
#'
#' @param num.data [\code{data.frame}]\cr
#'   A numeric data.frame
#' @param nfactors [\code{integer(1)}]\cr
#'   Amount of factors
#' @param rotate [\code{character(1L)}]\cr
#'   Rotation method used for factor analysis
#' @param par.vals [\code{list()}]\cr
#'   Other arguments to be passed to @seealso \code{\link[psych]{fa}}
#' @return [\code{list()}]
#'   A list containing the MDS analysis and plot
#' @import checkmate
#' @import BBmisc
#' @importFrom psych fa
#' @export
getFA = function(num.data, nfactors, rotate, par.vals) {
  fa.result = do.call(what = fa, args = append(list(r = num.data, nfactors = nfactors), par.vals))
  return(fa.result)
}
