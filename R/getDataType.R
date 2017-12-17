getDataType = function(data) {
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
  typelist = list(num = num, int = int, fact = fact,
    char = char, logic = logic, date = date)
  class(typelist) = append(class(typelist), "reportDataType")
  return(typelist)
}

##testing
data("diamonds", package = "ggplot2")
data("Boston", package = "MASS")
data("iris")

diamondstype = getDataType(diamonds)
diamondstype
iristype = getDataType(iris)
iristype
bostontype = getDataType(Boston)
bostontype

