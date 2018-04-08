#' @title Creates a FA Report Object
#'
#' @description
#' Directly creates a report object with one command
#'
#' @param id [\code{character(1)}]\cr
#'   ID of the Task Object. \code{\link{makeFATask}}
#' @param data [\code{data.frame}]\cr
#'   A Dataframe with different variables
#' @param ...
#'   as of now it does nothing
#'
#' @export
createFAReport = function(id, data, ...) {
  fa.task = makeFATask(id = id, data = data)
  fa.result = makeFA(fa.task)
  return(makeReport(fa.result))
}
