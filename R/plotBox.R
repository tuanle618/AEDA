#' Creates a box plot of all numerical features of a dataset
#' [WIP]
#' @param data [\code{data.frame}]\cr
#'   Data.
#' @param target [\code{character(1)}]\cr
#'   Target column. If no target is available in the dataset please insert \code{NULL}.
#'   If the target is numerical, the target will be mapped onto the x axis and the other numerical
#'   types will be mapped as boxplot onto the y axis
#' @param plot.x.only [\code{logical(1)}]\cr
#'   Logical whether only the numeric values should be ploted onto the x-axis. This option enables
#'   to turn on/turn off the "fill" argument in ggplot \code{aes(..., fill = target)}. Default is \code{FALSE}
#' @param col [\code{character(1)} | \code{integer(1)}]\cr
#'   Selected feature from \code{data}. If all features should be printed insert \code{NULL}.
#'   Default value is \code{NULL}
#' @param show.plot [\code{logical(1)}]\cr
#'   Logical whether the ggplot(s) should be displayed or not when executing this function.
#'   Default is \code{FALSE}
#' @param \dots other arguments to be passed to \link[ggplot2]{geom_boxplot}.
#' @return A ggplot2 object. Print it to plot it. [WIP if col is null]
#' @import checkmate
#' @import ggplot2
#' @import BBmisc
#' @import gridExtra
#' @examples
#'  data("diamonds", package = "ggplot2")
#'  boxplotdiamonds = plotBox(diamonds, target = "cut", col = "carat")
#'  data("iris")
#'  boxplotiris1 = plotBox(iris, target = "Species", show.plot = TRUE)
#'  boxplotiris2 = plotBox(iris, target = "Species", plot.x.only = TRUE, show.plot = TRUE)
#' @export
#' @title Creates a box plot of a numerical feature with respect to a target.


plotBox = function(data, target, plot.x.only = FALSE, col = NULL,
  show.plot = FALSE, ...) {

  assertDataFrame(data, col.names = "strict")
  if (!is.null(target)) {
    assertCharacter(target, min.len = 1)
  }
  if (is.null(target)) target = ""

  types = getDataType(data = data, target = target)
  numeric = unique(c(types$num, types$int))
  no.num = length(numeric)
  flag.target.factor = is.element(types$target, c(types$ord, types$fact, types$logic))

  ##plot.x.only : no filling with target
  if (plot.x.only) {
    #plot for a specific column:
    if (!is.null(col)) {
      if (is.numeric(col))  {
        col = colnames(data)[col]
      }
      x = data[[col]]
      #check if column is numeric:
      if (!(is.numeric(x))) stop("No Numeric Feature")
      #create the plot
      if (flag.target.factor) a = aes_string(x = "''", y = col)
      else a = aes_string(x = "''", y = col)
      plot = ggplot(data, a) + geom_boxplot(...) + coord_flip() + theme_classic()
      plot = list(plot = plot)
      names(plot) = col
    } else {
      #plot all numerical variables into one ggplot
      if (no.num == 0) stop("No Numeric Features")
      #create plots:
      plot = lapply(1:no.num, FUN = function(y) {
        col = numeric[y]
        if (is.factor(data[[target]])) a = aes_string(x = "''", y = col)
        else a = aes_string(x = "''", y = col)
        subplot = ggplot(data, a) + geom_boxplot(...) + coord_flip() + theme_classic()
        return(subplot)
      })
      names(plot) = numeric
    }
  } else {
  ##plot.x.only : filling with target
    #plot for a specific column:
    if (!is.null(col)) {
      if (is.numeric(col))  {
        col = colnames(data)[col]
      }
      x = data[[col]]
      #check if column is numeric:
      if (!(is.numeric(x))) stop("No Numeric Feature")
      #create the plot
      if (flag.target.factor) a = aes_string(x = "''", y = col, fill = target)
      else a = aes_string(x = "''", y = col)
      plot = ggplot(data, a) + geom_boxplot(...) + coord_flip() + theme_classic()
      plot = list(plot = plot)
      names(plot) = col
    } else {
      #plot all numerical variables into one ggplot
      if (no.num == 0) stop("No Numeric Features")
      #create plots:
      plot = lapply(1:no.num, FUN = function(y) {
        col = numeric[y]
        if (is.factor(data[[target]])) a = aes_string(x = "''", y = col, fill = target)
        else a = aes_string(x = "''", y = col)
        subplot = ggplot(data, a) + geom_boxplot(...) + coord_flip() + theme_classic()
        return(subplot)
      })
      names(plot) = numeric
    }
  }
  ##

  if (show.plot) {
    p = length(plot)
    pages = ceiling(p / 9)
    #split the plots by 6 such that on each page maximal 6 plots are displayed
    #use default
    if (p <= 6) {
      multiplot(plotlist = plot)
    } else {
      #suppressWarnings(do.call(grid.arrange, plot))
      multiplotPages(list = plot, k = 6, no.cols = 3)
    }
  }
  return(plot)
}
