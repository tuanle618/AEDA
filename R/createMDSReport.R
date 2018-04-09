#' @title Creates a MDS Report Object
#'
#' @description
#' Directly creates a report object with one command
#'
#' @param id [\code{character(1)}]\cr
#'   ID of the Task Object. \code{\link{makeMDSTask}}
#' @param data [\code{data.frame}]\cr
#'   A Dataframe with different variables
#' @param ...
#'   For now it does nothing
#'
#' @export
createMDSReport = function(id, data, ...) {
  mds.task = makeMDSTask(id = id, data = data, ...)
  mds.analysis.result = makeMDSAnalysis(mds.task)
  return(makeReport(mds.analysis.result))
}
