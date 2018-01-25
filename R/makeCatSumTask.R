#' @title Creates a CategoricalSummaryTask Object (for factor, ordered and logical columns)
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
#'@param \dots other arguments to be passed to \link[ggplot2]{geom_bar}.
#' @return CatSumTask object
#'
#' @import checkmate
#' @import BBmisc
#' @import vcd
#' @examples
#'  data("Arthritis", package = "vcd")
#'  cat.sum.task = makeCatSumTask(id = "Arthritis.Task", data = Arthritis, target = "Improved")
#'  #get Data
#'  cat.sum.task$env$data
#' @export
makeCatSumTask = function(id, data, target, ...){
  #Argument Checks
  assertCharacter(id, min.chars = 1L)
  assertDataFrame(data, col.names = "strict")
  #target will be checked within GetDataType

  # Encapsulate Data and Data Types into new env
  env = new.env(parent = emptyenv())
  env$data = data
  env$datatypes = getDataType(data, target)
  geombar.args = list(...)
  makeS3Obj("CatSumTask",
    id = id,
    type = "CategoricalSummary",
    env = env,
    size = nrow(data),
    catdatatypes = list(factor = env$datatypes$fac, ordered = env$datatypes$ord, logical = env$datatypes$logic),
    geombar.args = geombar.args
  )
}

#' @export
# Print fuction for NumTask Object
print.CatSumTask = function(x, ...) {
  catf("Task: %s", x$id)
  catf("Observations: %i", x$size)
  catf("Amount Factor Columns: %i", length(x$catdatatypes$fac))
  catf("Selected Factors: %s", collapse(x$catdatatypes$fac, sep = ", "))
  catf("Amount Ordered Columns: %i", length(x$catdatatypes$ordered))
  catf("Selected Ordered: %s", collapse(x$catdatatypes$ordered, sep = ", "))
  catf("Amount Logical Columns: %i", length(x$catdatatypes$logical))
  catf("Selected Logicals: %s", collapse(x$catdatatypes$logical, sep = ", "))
  catf("%s = %s", names(x$geombar.args), x$geombar.args)
}
