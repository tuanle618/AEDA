#' @title Creates a Correlation Report Object
#'
#' @description
#' Directly creates a report object with one command
#'
#' @param id [\code{character(1)}]\cr
#'   ID of the Task Object. \code{\link{makeCorrTask}}
#' @param data [\code{data.frame}]\cr
#'   A Dataframe with different variables
#' @param target [\code{character(1)}]\cr
#'   Target column. If not available please insert as \code{NULL}.
#' @param ...
#'   For now it does nothing
#'
#' @export
createCorrReport = function(id, data, ...) {
  my.task = makeCorrTask(id = "OpenMLReport", data = data)
  my.corr = makeCorr(my.task)
 return(makeCorrReport(my.corr))
}
