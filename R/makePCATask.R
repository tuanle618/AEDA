#' @title Creates a PCATask Object
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
#' @param exclude [\code{character}]
#'   Names of inputs, which should be excluded. Default is none.
#' @param vars [\code{character(1)}]\cr
#'  Column names
#' @param ...
#' Further arguments passed to \code{\link[stats]{prcomp}}
#'
#' @return PCATask
#'
#' @examples
#' data("iris")
#' pca.task = makePCATask(id = "iris.try", data = iris, target = "Species",
#'                         tol = 1e-1, center = TRUE)
#' # get Data
#' pca.task$env$data
#' @import checkmate
#' @import BBmisc
#' @import stats
#' @export
#'
makePCATask = function(id, data, target, vars = NULL, exclude = character(0), ...){
  # Argument Checks
  assertCharacter(id, min.chars = 1L)
  assertDataFrame(data, col.names = "strict")

  if (exists("target")) {
    if (!is.null(target)) {
      assertCharacter(target, len = 1)
      assertChoice(target, colnames(data))
    }
  } else if (!exists("target")) {
    stop("You did not specify a target value. If the dataset doesn't contain one, enter NULL as target")
  }

  if (!is.null(vars)) {
    assertCharacter(vars, min.chars = 1L, min.len = 2L)
    data.type = getDataType(data[, vars], target = target)
  } else{
    data.type = getDataType(data, target = target)
  }

  #check if at least 3 numeric columns are in the dataset
  if (length(data.type$num) + length(data.type$int) <= 2) {
    stop(paste("The dataset only contains", length(data.type$num) + length(data.type$int), "numeric columns.
      Principal Component Analysis only makes sense for a data set with at least 3 numeric variables."))
  }

  #target will be checked within GetDataType
  #for target if it is numeric exclude it
  num.features = data.type[c("num", "int")]
  num.features = setdiff(unlist(num.features), target)

  # Encapsulate Data and Data Types into new env
  env = new.env(parent = emptyenv())
  env$data = data

  makeS3Obj("PCATask",
    id = id,
    type = "PCA",
    env = env,
    features = num.features,
    size = nrow(data),
    exclude = exclude,
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
  if (length(x$exclude) > 0) {
    catf("Exclude: %s", as.character(x$exclude))
  }
  catf("Observations: %i", x$size)
  catf("Missing Values: %s", x$missing.values)
  catf("Additional parameters to prcomp:")
  catf("%s = %s ", names(x$pca.args), x$pca.args)
}

