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
    if (!is.null(target)) {
      assertCharacter(target, len = 1)
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
  for (i in 1:ncol(data)) {
    coldata = data[,i]
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
    num = num, int = int, ord = ord, fact = fact,
    char = char, logic = logic, date = date)
  addClasses(typelist, "reportDataType")
  return(typelist)
}

##Testing:
#data("diamonds", package = "ggplot2")
#dlist1 = getDataType(diamonds, target = "price")
##look ok
#diamonds$cut = as.factor(as.character(diamonds$cut))
#diamonds$clarity = as.character(diamonds$clarity)
#dlist2 = getDataType(diamonds, target = "price")
#ok
