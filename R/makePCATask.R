#' @title Creates a PCATask Objects
#'
#' @description
#' Principal Components Analysis (PCA). A Task encapsulates the Data with some additional information
#'
#' @param id [\code{character(1)}]\cr
#'   ID of the Task Object
#' @param data [\code{data.frame}]\cr
#'   Data for PCA. Only numeric columns will be used and the target column excluded.
#' @param target [\code{character(1)}]\cr
#'   Target column. If not available please insert as \code{NULL}.
#' @param method [\code{character(1)}]\cr
#'   Defines the PCA method
#'   \dQuote{all}
#' @param exclude [\code{character}]\cr
#'   Names of inputs, which should be excluded. Default is none.
#' @param scale [\code{logical(1)}]\cr
#'  Whether the variables should be scaled to have unit variance before the analysis takes place.
#'  The default is \code{TRUE}
#' @param vars [\code{character(1)}]\cr
#'  Column names
#' @param ...
#' Further arguments passed to \code{\link[stats]{prcomp}}
#'
#' @return PCATask
#'
#' @examples
#' data("iris")
#' test.task = makePCATask(id = "Probe", data = iris, target = "Petal.Lenght",
#'             scale = TRUE, tol = 1e-1)
#' # get Data
#' test.task$env$data
#' @import checkmate
#' @import BBmisc
#' @import stats
#' @export
#'
makePCATask = function(id, data, target, method = "all", vars = NULL, exclude = character(0), scale = TRUE, ...){
  # Argument Checks
  assertCharacter(id, min.chars = 1L)
  assertDataFrame(data, col.names = "strict")
  assertSubset(method, c("all", "linear", "nonlinear"), empty.ok = FALSE)
  if (!is.null(vars)) {
    assertCharacter(vars, min.chars = 1L, min.len = 2L)
    data.type = getDataType(data[, vars], target = NULL)
  } else{
    data.type = getDataType(data, target = NULL)
  }

  #target will be checked within GetDataType

  # Encapsulate Data and Data Types into new env
  env = new.env(parent = emptyenv())
  env$data = data

  makeS3Obj("PCATask",
    id = id,
    type = "PCA",
    env = env,
    features = data.type[c("num", "int")],
    size = nrow(data),
    method = method,
    missing.values = sum(is.na(data)),
    pca.args = list(...)
  )
}

#' @export
# Print fuction for PCATask Object
print.PCATask = function(x, ...) {
  catf("Task: %s", x$id)
  catf("Type: %2s", x$type)
  catf("Selected Features: %s", collapse(unlist(x$features), sep = ", "))
  catf("Observations: %i", x$size)
  catf("Method: %s", x$method)
  catf("Missing Values: %s", x$missing.values)
  catf("pca.args: %s", x$pca.args)
}

