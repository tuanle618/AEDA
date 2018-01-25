#' Creates a Density plot of a numerical feature or all numerical features of a dataset .
#'
#' @param data [\code{data.frame}]\cr
#'   Data.
#' @param target [\code{character(1)}]\cr
#'   Target column. If no target is available in the dataset please insert \code{NULL}
#' @param col [\code{character(1)} | \code{integer(1)}]\cr
#'   Selected feature from \code{data}. If all numerical features should be printed insert \code{NULL}.
#'   Default value is \code{NULL}
#' @param show.plot [\code{logical(1)}]\cr
#'   Logical whether the ggplot(s) should be displayed or not when executing this function.
#'   Default is \code{FALSE}
#' @param alpha [\code{numeric(1)}]\cr
#'   Default is \code{alpha = 0.4}
#' @param colour [\code{character(1)}]\cr
#'   Default is  \code{colour = "black"}
#' @param \dots other arguments to be passed to \link[ggplot2]{geom_density}.
#' @return A ggplot2 object. Print it to plot it. [WIP if col is null]
#' @import checkmate
#' @import ggplot2
#' @import BBmisc
#' @import gridExtra
#' @examples
#'  data("Boston", package = "MASS")
#'  gghistplot = plotDens(Boston, target = "medv", col = "age")
#'  gghistAllplot = plotDens(Boston, target = "medv", col = NULL)
#' @export
#' @title Creates a density plot for one/ all numerical features(s) of a dataset.


plotDens = function(data, target, col = NULL, show.plot = FALSE, alpha = 0.4, colour = "black", ...) {
  #add.args = list(...)
  assertNumber(alpha)
  assertCharacter(colour)
  assertDataFrame(data, col.names = "strict")
  if (!is.null(target)) {
    assertCharacter(target, min.len = 1)
  }
  #plot for a specific column:
  if (!is.null(col)) {
    if (is.numeric(col))  {
      col = colnames(data)[col]
    }
    x = data[[col]]
    #check if column is numerical:
    if (!(is.numeric(x) | is.integer(x))) stop("No Numeric Feature")
    #create the plot
    a = aes_string(x = col, colour = target)
    plot = ggplot(data, a) + geom_density(aes(y = ..density..), alpha = alpha, colour = colour, ...)
    plot = list(plot = plot)
    names(plot) = col
  } else {
    #plot all numerical variables into several ggplots
    types = getDataType(data = data, target = target)
    num = unique(c(types$num, types$int))
    no.num = length(num)
    if (no.num == 0) stop("No Numeric Features")
    #create plots:
    plot = lapply(1:no.num, FUN = function(y) {
      col = num[y]
      a = aes_string(x = col, colour = target)
      subplot = ggplot(data, a) + geom_density(aes(y = ..density..), alpha = alpha, colour = colour, ...)
      return(subplot)
    })
    names(plot) = num
  }
  if (show.plot) {
    p = length(plot)
    pages = ceiling(p / 9)
    #split the plots by 6 such that on each page maximal 6 plots are displayed
    #if max 3 plots, use ncol = 1, nrow = 3
    if (p <= 6) {
      multiplot(plotlist = plot)
    } else {
      #suppressWarnings(do.call(grid.arrange, plot))
      AEDA::multiplotPages(list = plot, k = 6, no.cols = 3)
    }
  }
  return(plot)
}
