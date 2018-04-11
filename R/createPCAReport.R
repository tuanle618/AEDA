#' @title Creates a PCA Report Object
#'
#' @description
#' Directly creates a report object with one command
#'
#' @param id [\code{character(1)}]\cr
#'   ID of the Task Object. \code{\link{makePCATask}}
#' @param data [\code{data.frame}]\cr
#'   A Dataframe with different variables
#' @param target [\code{character(1)}]\cr
#'   Target column. If not available please insert as \code{NULL}.
#' @param ...
#'   Further arguments for makePCA
#'
#' @export
createPCAReport = function(id, data, target, ...) {
  pca.task = makePCATask(id = id, data = data,
    target = target, ...)
  pca.result = makePCA(pca.task)
  return(makeReport(pca.result))
}

