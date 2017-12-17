#' Get Data Types from a data.frame.
#'
#' @param data [\code{data.frame}]\cr
#'   Data to extract the types from
#' @param target [\code{character(1)}]\cr
#'   Target column. If not available please insert as \code{NULL}.
#' @examples
#' data("iris")
#' getDataType(iris)
#' @return [named \code{list}], containing vectors according to types. Vectors contain column names of the data.frame
#'
getDataType = function(data, target) {
  #Argument checking
  assertDataFrame(data)
  if (exists("target")) {
    if (is.null(target)) {
      target = NULL
    } else {
      assertCharacter(target, len = 1)
    }
  } else if (!exists("target")) {
    stop("You did not specify a target value. If the dataset doesn't contain one, enter NULL as target")
  }

  #Initialize vectors which contain colnames
  num = vector(mode = "character")
  int = vector(mode = "character")
  fact = vector(mode = "character")
  char = vector(mode = "character")
  logic = vector(mode = "character")
  date = vector(mode = "character")
  #Loop through each column and get class
  for (i in 1:ncol(data)) {
    coldata = data[,i]
    colname = colnames(data)[i]
    if (is.numeric(coldata)) {
      num   = c(num, colname)
    } else if (is.integer(coldata)) {
      int = c(int, colname)
    } else if (is.factor(coldata) || is.ordered(coldata)) {
      fact  = c(fact, colname)
    } else if (is.character(coldata)) {
      char  = c(char, colname)
    } else if (is.logical()) {
      logic = c(logic, colname)
    } else if (class(coldata) == "Date") {
      date = c(date, colname)
    }
  }
  if (!is.null(target)) {
    targetidx = which(colnames(data) == target)
    X = colnames(data)[-targetidx]
  } else X = colnames(data)

  typelist = list(X = X, target = target,
    num = num, int = int, fact = fact,
    char = char, logic = logic, date = date)
  class(typelist) = append(class(typelist), "reportDataType")
  return(typelist)
}
