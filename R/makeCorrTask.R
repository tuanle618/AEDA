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
#' @param vars [\code{character(1)}]\cr
#'    Column names to use for correlation
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
makeCorrTask = function(id, data, method = "pearson", vars = NULL){
  # Argument Checks
  assertCharacter(id, min.chars = 1L)
  assertDataFrame(data, col.names = "strict")

  assertSubset(method, c("pearson", "spearman"), empty.ok = FALSE)
  if (!is.null(vars)) {
    assertCharacter(vars, min.chars = 1L, min.len = 2L)
    data.type = getDataType(data[, vars], target = NULL)
  } else{
    data.type = getDataType(data, target = NULL)
  }
  # Encapsulate Data into new env
  env = new.env(parent = emptyenv())
  env$data = data


  makeS3Obj("CorrTask",
    id = id,
    type = "Correlation",
    env = env,
    features = data.type[c("num", "int")],
    size = nrow(data),
    method = method,
    missing.values = sum(is.na(data)))
}

#' @export
# Print fuction for CorrTask Object
print.CorrTask = function(x, ...) {
  catf("Task: %s", x$id)
  catf("Type: %2s", x$type)
  catf("Selected Features: %s", collapse(unlist(x$features), sep = ", "))
  catf("Observations: %i", x$size)
  catf("Method: %s", x$method)
  catf("Missing Values: %s", x$missing.values)
}
