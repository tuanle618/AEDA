#' @title Creates a Basic Report Object
#' @param id [\code{character(1)}]\cr
#'   ID of the Task Object. \code{\link{makeBasicReportTask}}
#' @param data [\code{data.frame}]\cr
#'   A Dataframe with different variables
#' @param target [\code{character(1)}]\cr
#'   Target column. If not available please insert as \code{NULL}.
#'
#' @export
createBasicReport = function(id, data, target) {
  task = makeBasicReportTask(id = id, data = data, target = target)
  return(makeReport(task))
}
