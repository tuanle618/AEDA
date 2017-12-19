#' @title Creates a CorrTask Objects
#'
#' @description
#' A Task encapsulates the Data with some additional information
#'
#' @param id [\code{character(1)}]\cr
#'   ID of the Task Object
#' @param data [\code{data.frame}]\cr
#'   A Dataframe with different variables
#' @param method [\code{character(1)}]\cr
#'   Defines the correlation method
#'   Possible choices are:
#'   \dQuote{pearson}
#'
#'
#' @return CorrTask
#'
#' @examples
#' my.task = makeCorrTask(id = "test", data = cars)
#' # Extract Data
#' my.task$env$data
#' @import checkmate
#' @import BBmisc
#' @export
makeCorrTask = function(id, data, method = "pearson"){
  # Argument Checks
  assertCharacter(id, min.chars = 1L)
  assertDataFrame(data, col.names = "strict", any.missing = FALSE)
  assertSubset(method, c("pearson"), empty.ok = FALSE)
  # Encapsulate Data into new env
  env = new.env(parent = emptyenv())
  env$data = data

  makeS3Obj("CorrTask",
    id = id,
    type = "Correlation",
    env = env,
    size = nrow(data),
    method = method,
    missing.values = FALSE)
}

#' @export
# Print fuction for CorrTask Object
print.CorrTask = function(x, ...) {
  catf("Task: %s", x$id)
  catf("Type: %s", x$type)
  catf("Observations: %i", x$size)
  catf("Method: %s", x$method)
  catf("Missing Values: %s", x$missing.values)
}
