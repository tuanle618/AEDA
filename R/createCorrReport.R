#' @title Creates a Correlation Report Object
#'
#' @description
#' Directly creates a report object with one command
#'
#' @param id [\code{character(1)}]\cr
#'   ID of the Task Object. \code{\link{makeCorrTask}}
#' @param data [\code{data.frame}]\cr
#'   A Dataframe with different variables
#' @param ...
#'   For now it does nothing
#'
#' @export
createCorrReport = function(id, data, ...) {
  corr.task = makeCorrTask(id = "OpenMLReport", data = data, ...)
  corr.result = makeCorr(corr.task)
 return(makeCorrReport(corr.result))
}

