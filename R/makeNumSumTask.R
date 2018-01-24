#' @title Creates a NumericSummaryTask Object (for numeric and integer columns)
#'
#' @description
#' A Task encapsulates the Data with some additional information
#'
#' @param id [\code{character(1)}]\cr
#'   ID of the Task Object
#' @param data [\code{data.frame}]\cr
#'   A Dataframe with different variables
#' @param target [\code{character(1)}]\cr
#'  Target column. If not available please insert as \code{NULL}.
#' @return NumSumTask object
#'
#' @import checkmate
#' @import BBmisc
#' @examples
#'  data("Boston", package = "MASS")
#'  num.sum.task = makeNumSumTask(id = "BostonTask", data = Boston, target = "medv")
#'  #get Data
#'  num.sum.task$env$data
#' @export
makeNumSumTask = function(id, data, target){
  #Argument Checks
  assertCharacter(id, min.chars = 1L)
  assertDataFrame(data, col.names = "strict")
  #target will be checked within GetDataType

  # Encapsulate Data and Data Types into new env
  env = new.env(parent = emptyenv())
  env$data = data
  env$datatypes = getDataType(data, target)

  makeS3Obj("NumSumTask",
    id = id,
    type = "NumericSummary",
    env = env,
    size = nrow(data),
    numdatatypes = list(numeric = env$datatypes$num, integer = env$datatypes$int)
    )
}

#' @export
# Print fuction for NumTask Object
print.NumSumTask = function(x, ...) {
  catf("Task: %s", x$id)
  catf("Observations: %i", x$size)
  catf("Amount Numeric Columns: %i", length(x$numdatatypes$numeric))
  catf("Selected Numerics: %s", collapse(x$numdatatypes$numeric, sep = ", "))
  catf("Amount Integer Columns: %i", length(x$numdatatypes$integer))
  catf("Selected Integers: %s", collapse(x$numdatatypes$integer, sep = ", "))
}
