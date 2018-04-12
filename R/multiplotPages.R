#' @title Wrapper for multiplot
#'
#' @description
#' Wrapper for multiplot with further plotlist splitting
#'
#' @param plotlist [\code{list()}]\cr
#'   A list of ggplot objects. Default is \code{plotlist = NULL}
#' @param k [\code{matrix()}]\cr
#'   Maximum number of plots on one page
#' @param no.cols [\code{integer(1)}]\cr
#'   Maximum number of columns
#' @param ...
#' Further arguments are handed over to multiplot
#'
#' @return NULL
#' @import checkmate
#' @export
multiplotPages = function(plotlist, k, no.cols, ...) {
  assertList(plotlist, null.ok = FALSE)
  for (i in seq_along(plotlist)) {
    assertClass(plotlist[[i]], classes = c("gg", "ggplot"))
  }
  assertInt(k, null.ok = FALSE)

  #create sublists which act as page
  splitted.list = splitGGplotList(plotlist, k)
  for (page in seq_len(length(splitted.list))) {
    ##call multiplot with further arguments
    #calculate optimal no. of cols
    if (length(splitted.list[[page]]) < no.cols) {
      sel.cols = length(splitted.list[[page]])
    } else {
      sel.cols = no.cols
    }
    multiplot(plotlist = splitted.list[[page]],
      cols = sel.cols, ...)
  }
}
