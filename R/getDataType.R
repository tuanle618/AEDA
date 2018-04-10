#' Get Data Types from a data.frame.
#'
#' @param data [\code{data.frame}]\cr
#'   Data to extract the types from
#' @param target [\code{character(1)}]\cr
#'   Target column. If not available please insert as \code{NULL}.
#' @examples
#' data("iris")
#' getDataType(iris, target = NULL)
#' @return [named \code{list}], containing vectors according to types. Vectors contain column names of the data.frame
#' @export
getDataType = function(data, target) {
  #Argument checking
  assertDataFrame(data)
  if (exists("target")) {
    if (!is.null(target)) {
      assertCharacter(target, len = 1)
      assertChoice(target, colnames(data))
    }
  } else if (!exists("target")) {
    stop("You did not specify a target value. If the dataset doesn't contain one, enter NULL as target")
  }

  #Initialize vectors which contain colnames
  num = vector(mode = "character")
  int = vector(mode = "character")
  fact = vector(mode = "character")
  ord = vector(mode = "character")
  char = vector(mode = "character")
  logic = vector(mode = "character")
  date = vector(mode = "character")
  #Loop through each column and get class
  for (i in seq_len(ncol(data))) {
    coldata = data[[i]]
    colname = colnames(data)[i]
    if (is.integer(coldata)) {
      int   = c(int, colname)
    } else if (is.numeric(coldata)) {
      num = c(num, colname)
    } else if (is.factor(coldata) || is.ordered(coldata)) { #check if column is either ordered or factored
      if (is.factor(coldata) & is.ordered(coldata)) { #check if column is both factored and ordered. then assign ordered
        ord = c(ord, colname)
      } else {
          if (is.ordered(coldata)) {
            ord = c(ord, colname) #check if only ordered, then assign ordered
          }
          else {
            fact  = c(fact, colname)
          }
      }
    } else if (is.character(coldata)) {
      char  = c(char, colname)
    } else if (is.logical(coldata)) {
      logic = c(logic, colname)
    } else if (class(coldata) == "Date") {
      date = c(date, colname)
    }
  }
  if (!is.null(target)) {
    targetidx = which(colnames(data) == target)
    X = colnames(data)[-targetidx]
  } else X = colnames(data)


  makeS3Obj("reportDataType",
    X = X,
    target = target,
    num = num,
    int = int,
    ord = ord,
    fact = fact,
    char = char,
    logic = logic,
    date = date
  )
}

##Testing:
#data("diamonds", package = "ggplot2")
#dlist1 = getDataType(diamonds, target = "price")
##look ok
#diamonds$cut = as.factor(as.character(diamonds$cut))
#diamonds$clarity = as.character(diamonds$clarity)
#dlist2 = getDataType(diamonds, target = "price")
#ok

##Include print method for reportDataType
#' @export
print.reportDataType = function(x, ...) {
  catf("X-Variables: %s", collapse(unlist(x$X), sep = ", "))
  catf("Target: %2s", x$target)
  if (length(x$num > 0)) catf("Numeric Variables: %s", collapse(unlist(x$num), sep = ", "))
  if (length(x$int > 0)) catf("Integer Variables: %s", collapse(unlist(x$int), sep = ", "))
  if (length(x$ord > 0)) catf("Ordered Variables: %s", collapse(unlist(x$ord), sep = ", "))
  if (length(x$fact > 0)) catf("Categorical Variables: %s", collapse(unlist(x$fact), sep = ", "))
  if (length(x$char > 0)) catf("Character Variables: %s", collapse(unlist(x$char), sep = ", "))
  if (length(x$logic > 0)) catf("Logic Variables: %s", collapse(unlist(x$logic), sep = ", "))
  if (length(x$date > 0)) catf("Date Variables: %s", collapse(unlist(x$date), sep = ", "))
}
