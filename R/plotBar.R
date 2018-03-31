#' Creates a bar plot of a categorical feature or all categorical features of a dataset .
#'
#' @param data [\code{data.frame}]\cr
#'   Data.
#' @param target [\code{character(1)}]\cr
#'   Target column. If no target is available in the dataset please insert \code{NULL}
#' @param col [\code{character(1)} | \code{integer(1)}]\cr
#'   Selected feature from \code{data}. If all categorical features should be printed insert \code{NULL}.
#'   Default value is \code{NULL}
#' @param show.plot [\code{logical(1)}]\cr
#'   Logical whether the ggplot(s) should be displayed or not when executing this function.
#'   Default is \code{FALSE}
#' @param position [\code{character(1)}]\cr
#'   Position how to display the bar plot. Default is \code{position = "dodge"}
#' @param \dots other arguments to be passed to \link[ggplot2]{geom_bar}.
#' @return A ggplot2 object. Print it to plot it. [WIP if col is null]
#' @import checkmate
#' @import ggplot2
#' @import BBmisc
#' @import gridExtra
#' @examples
#'  data("Arthritis", package = "vcd")
#'  ggplot = plotBar(Arthritis, target = "Improved")
#' @export
#' @title Creates a bar plot of a categorical (discrete) feature.


plotBar = function(data, target, col = NULL, show.plot = FALSE, position = "dodge", ...) {
  assertCharacter(position)
  assertDataFrame(data, col.names = "strict")
  if (!is.null(target)) {
    assertCharacter(target, min.len = 1)
    assertChoice(target, choices = colnames(data))
    #check if column is a factor:
    assert_factor(data[[target]])
  }


  #plot for a specific column:
  if (!is.null(col)) {
    if (is.numeric(col))  {
      col = colnames(data)[col]
    }
    x = data[[col]]
    #check if column is categorical:
    if (!(is.factor(x) | is.logical(x) | is.character(x) | is.ordered(x))) stop("No Discrete Feature")
    #create the plot
    a = aes_string(x = col, fill = target)
    plot = ggplot(data, a) + theme_classic() +
      geom_bar(stat = "count", position = position, ...) + coord_flip()
    plot = list(plot = plot)
    names(plot) = col
  } else {
    #plot all categorical variables into one ggplot
    types = getDataType(data = data, target = target)
    categ = unique(c(types$ord, types$fact, types$logic))
    no.categ = length(categ)
    if (no.categ == 0) stop("No Discrete Features")
    #create plots:
    plot = lapply(1:no.categ, FUN = function(y) {
      col = categ[y]
      a = aes_string(x = col, fill = target)
      subplot = ggplot(data, a) + theme_classic() +
        geom_bar(stat = "count", position = position, ...) + coord_flip()
      return(subplot)
    })
    names(plot) = categ
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
      multiplotPages(list = plot, k = 6, no.cols = 3)
    }
  }
  return(plot)
}
