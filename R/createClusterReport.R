#' @title Creates a Cluster Report Object
#'
#' @description
#' Directly creates a report object with one command
#'
#' @param id [\code{character(1)}]\cr
#'   ID of the Task Object. \code{\link{makeClusterTask}}
#' @param data [\code{data.frame}]\cr
#'   A Dataframe with different variables
#' @param ...
#'   For now it does nothing
#'
#' @export
createClusterReport = function(id, data, ...) {
  cluster.task = makeClusterTask(id = id, data = data, ...)
  cluster.analysis.result = makeClusterAnalysis(cluster.task)
  return(makeReport(cluster.analysis.result))
}

