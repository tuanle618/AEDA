#' @title Creates a MDS Report Object
#' @param id [\code{character(1)}]\cr
#'   ID of the Task Object. \code{\link{makeMDSTask}}
#' @param data [\code{data.frame}]\cr
#'   A Dataframe with different variables
#' @param ...
#'   For now it does nothing
#'
#' @export
createMDSReport = function(id, data, ...) {
  my.mds.task = makeMDSTask(id = id, data = data)
  mds.analysis = makeMDSAnalysis(my.mds.task)
  return(makeReport(mds.analysis))
}
