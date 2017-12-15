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
    type = class(coldata)
    print(type)
    #Problems: is.numeric(1L) and is.numeric(1) both true.. really need to seperate between int and num
    if (type == "numeric") {
      num   = c(num, colname)
    } else if (type == "integer") {
      int = c(int, colname)
    } else if (is.factor(coldata) || is.ordered(coldata)) { ##ordered factor has to be improved..
      fact  = c(fact, colname)
    } else if (type == "character") {
      char  = c(char, colname)
    } else if (type == "logical") {
      logic = c(logic, colname)
    } else if (type == "Date") {
      date = c(date, colname)
    }
  }
  typelist = list(num = num, int = int, fact = fact,
    char = char, logic = logic, date = date)
}

##testing
data("diamonds", package = "ggplot2")
data("Boston", package = "MASS")
data("iris")

diamondstype = getDataType(diamonds) #spuckt fehler aus
iristype = getDataType(iris)
bostontype = getDataType(Boston)

