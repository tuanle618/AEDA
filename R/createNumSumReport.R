#' @title Creates a NumSum Report Object
#'
#' @description
#' Directly creates a report object with one command
#'
#' @param id [\code{character(1)}]\cr
#'   ID of the Task Object. \code{\link{makeNumSumTask}}
#' @param data [\code{data.frame}]\cr
#'   A Dataframe with different variables
#' @param target [\code{character(1)}]\cr
#'   Target column. If not available please insert as \code{NULL}.
#' @param ...
#'   Further arguments for makeNumSumTask
#'
#' @export
createNumSumReport = function(id, data, target, ...) {
  num.sum.task = makeNumSumTask(id = id, data = data,
    target = target, ...)
  num.sum.result = makeNumSum(num.sum.task)
  return(makeReport(num.sum.result))
}

