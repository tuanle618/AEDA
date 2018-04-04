#' @title Creates a CatSum Report Object
#' @param id [\code{character(1)}]\cr
#'   ID of the Task Object. \code{\link{makeCatSumTask}}
#' @param data [\code{data.frame}]\cr
#'   A Dataframe with different variables
#' @param target [\code{character(1)}]\cr
#'   Target column. If not available please insert as \code{NULL}.
#' @param ...
#'   Further arguments for makeCatSumTask
#'
#' @export
createCatSumReport = function(id, data, target, ...) {
  cat.sum.task = makeCatSumTask(id = id, data = data, target = target, ...)
  cat.sum = makeCatSum(cat.sum.task)
  return(makeCatSumReport(cat.sum))
}
