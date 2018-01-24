#' Plots univariate distribution of a feature.
#'
#' @param data [\code{data.frame}]\cr
#'   Data.
#' @param target [\code{character(1)}]\cr
#'   Target column. If no target is available in the dataset please insert \code{NULL}
#' @param col [\code{character(1)} | \code{integer(1)}]\cr
#'   Selected feature from \code{data}.
#' @return A ggplot2 object. Print it to plot it.
#' @import checkmate
#' @import ggplot2
#' @import BBmisc
#' @examples
#'  data("diamonds", package = "ggplot2")
#'  ggplot = plotFeatDistr(diamonds, target = "cut", col = "carat")
#' @export
#' @title Plots univariate distribution of a feature.


plotFeatDistr = function(data, target, col) {
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
    plot = ggplot(data, a) +
           geom_density(size = 2) +
           geom_histogram(aes(y = ..density..), bins = 30, alpha = 0.4)
  } else if (is.factor(x) | is.logical(x) | is.character(x)) {
      a = aes_string(x = col, fill = target)
      plot = ggplot(data, a) +
             geom_bar(stat = "count", position = "dodge") ##can be very big for many classes otherwise "stack"
    } else {
        stop("Unsupported column class: ", class(x))
      }
}
