#' Plots univariate distribution of a feature.
#'
#' @param data [\code{data.frame}]\cr
#'   Data.
#' @param target [\code{character(1)}]\cr
#'   Target column. If no target is available in the dataset please insert \code{NULL}
#' @param col [\code{character(1)} | \code{integer(1)}]\cr
#'   Selected feature from \code{data}.
#' @param geom.hist.args [\code{list()}] \cr
#'  Other arguments to be passed to \link[ggplot2]{geom_histogram}.
#' @param geom.dens.args [\code{list()}] \cr
#'  Other arguments to be passed to \link[ggplot2]{geom_density}.
#' @return A ggplot2 object. Print it to plot it.
#' @import checkmate
#' @import ggplot2
#' @import BBmisc
#' @examples
#'  data("diamonds", package = "ggplot2")
#'  ggplot = plotFeatDistr(diamonds, target = "cut", col = "carat")
#' @export
#' @title Plots univariate distribution of a feature.


plotFeatDistr = function(data, target, col, geom.hist.args, geom.dens.args) {
  if (is.numeric(col))  {
    col = colnames(data)[col]
  }
  assertDataFrame(data, col.names = "strict")
  if (!is.null(target)) {
    assertCharacter(target, min.len = 1)
  }
  x = data[[col]]
  if (is.numeric(x) | is.integer(x)) {
    a = aes_string(x = col, colour = target)
    geom.hist.wrapper = do.call(geom_histogram, append(list(mapping = aes(y = ..density..)), geom.hist.args))
    geom.dens.wrapper = do.call(geom_density, geom.dens.args)
    plot = ggplot(data, a) + geom.hist.wrapper + geom.dens.wrapper
    return(plot)
  } else {
        stop("Unsupported column class: ", class(x))
    }
}
