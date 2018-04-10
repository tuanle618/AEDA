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
#'   \dQuote{pearson}, \dQuote{spearman}, \dQuote{kendall} \cr
#'   Default method is \code{method = "pearson"}
#' @param vars [\code{character(1)}]\cr
#'   Column names to use for correlation
#' @param type [\code{character(1)}]\cr
#'   The type of the Report to create. Example: "CorrPlot"
#' @param show.NA.msg [\code{logical(1)}]\cr
#'  Logical whether to show missing values message\cr
#'  Default is \code{FALSE}.
#' @param ...
#'  For now has no use
#' @return CorrTask
#'
#' @examples
#' my.task = makeCorrTask(id = "test", data = cars)
#' # Extract Data
#' my.task$env$data
#' @import checkmate
#' @import BBmisc
#' @export
makeCorrTask = function(id, data, method = "pearson", vars = NULL,
  type = "CorrPlot", show.NA.msg = FALSE, ...){
  # Argument Checks
  assertCharacter(id, min.chars = 1L)
  assertDataFrame(data, col.names = "strict")
  assertSubset(method, c("pearson", "spearman", "kendall"), empty.ok = FALSE)
  assertSubset(type, choices = "CorrPlot")
  #add warning for NAs:
  if (any(is.na(data)) & show.NA.msg) {
    message("The data set contains NAs.
These values might removed in the further calculations.
If so, another warning will be displayed.")
  }
  if (!is.null(vars)) {
    assertCharacter(vars, min.chars = 1L, min.len = 2L)
    data.type = getDataType(data[, vars], target = NULL)
  } else{
    data.type = getDataType(data, target = NULL)
  }
  # Encapsulate Data into new env
  env = new.env(parent = emptyenv())
  env$data = data

  # For pearson no ordinal features
  if (method == "pearson") {
    data.types = data.type[c("num", "int")]
  } else {
    data.types = data.type[c("num", "int", "ord")]
  }

  makeS3Obj("CorrTask",
    id = id,
    env = env,
    features = data.types,
    size = nrow(data),
    method = method,
    data.name = deparse(substitute(data)),
    needed.pkgs = NULL,
    missing.values = sum(is.na(data)),
    type = type)
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
  catf("Name of the Data: %s", x$data.name)
  catf("Seleted type : %s", x$type)
  catf("Needed packages: %s", if (is.null(x$needed.pkgs)) {"None"} else{x$needed.pkgs})
}

