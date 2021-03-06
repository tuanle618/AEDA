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
#' @param geom.hist.args [\code{list()}] \cr
#'  Other arguments to be passed to \link[ggplot2]{geom_histogram}.
#'  Default is \code{list(bins = 30, alpha = 0.4)}
#' @param geom.dens.args [\code{list()}] \cr
#'  Other arguments to be passed to \link[ggplot2]{geom_density}.
#'  Default is \code{list(size = 2, alpha = 0.4)}
#' @param geom.box.args [\code{list()}] \cr
#'  Other arguments to be passed to \link[ggplot2]{geom_boxplot}.
#'  Default is \code{NULL}. Default args from \link[ggplot2]{geom_boxplot} will be passed.
#' @param show.NA.msg [\code{logical(1)}]\cr
#'  Logical whether to show missing values message\cr
#'  Default is \code{FALSE}.
#' @param ...
#'  For now has no use
#' @return NumSumTask object
#'
#' @import checkmate
#' @import BBmisc
#' @examples
#'  data("Aids2", package = "MASS")
#'  num.sum.task = makeNumSumTask(id = "AidsTask", data = Aids2,
#'    target = "sex")
#'  #get Data
#'  num.sum.task$env$data
#' @export
makeNumSumTask = function(id, data, target, geom.hist.args = list(bins = 30, alpha = 0.4),
  geom.dens.args = list(size = 2, alpha = 0.4), geom.box.args = list(),
  show.NA.msg = FALSE, ...){
  #Argument Checks
  assertCharacter(id, min.chars = 1L)
  assertDataFrame(data, col.names = "strict")
  assertList(geom.hist.args)
  assertList(geom.dens.args)
  assertList(geom.box.args)
  #target will be checked within GetDataType

  #add warning for NAs:
  if (any(is.na(data)) & show.NA.msg) {
    message("The data set contains NAs.
These values might removed in the further calculations.
If so, another warning will be displayed.")
  }
  # Encapsulate Data and Data Types into new env
  env = new.env(parent = emptyenv())
  env$data = data
  env$datatypes = getDataType(data, target)

  makeS3Obj("NumSumTask",
    id = id,
    type = "NumericSummary",
    env = env,
    size = nrow(data),
    numdatatypes = list(numeric = env$datatypes$num, integer = env$datatypes$int),
    geom.hist.args = geom.hist.args,
    geom.dens.args = geom.dens.args,
    geom.box.args = geom.box.args
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
  if (length(x$geom.hist.args) != 0) {
    catf("Additional params for geom_histogram(): %i", length(x$geom.hist.args))
    print(unlist(x$geom.hist.args))
  }
  if (length(x$geom.dens.args) != 0) {
    catf("Additional params for geom_density(): %i", length(x$geom.dens.args))
    print(unlist(x$geom.dens.args))
  }
  if (length(x$geom.box.args) != 0) {
    catf("Additional params for geom_boxplot(): %i", length(x$x$geom.box.args))
    print(unlist(x$geom.box.args))
  }
}
