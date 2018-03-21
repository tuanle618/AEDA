#' @title Calculates a Categorical Summary
#'
#' @description
#' makeCatSum calculates a categorical summary and wrapes an object around it
#'
#' @param cat.sum.task [\code{CatSumTask}]\cr
#'   A CatSumTask Object
#' @return CatSumObj
#' @examples
#'  data("Arthritis", package = "vcd")
#'  cat.sum.task = makeCatSumTask(id = "Arthritis.Task", data = Arthritis, target = "Improved")
#'  #get the categorical summary task object
#'  cat.sum = makeCatSum(cat.sum.task)
#' @import checkmate
#' @import BBmisc
#' @import moments
#' @import stats
#' @import vcd
#' @export
makeCatSum = function(cat.sum.task){
  assertClass(cat.sum.task, "CatSumTask")

  data = cat.sum.task$env$data
  features = unlist(cat.sum.task$catdatatypes)
  target = cat.sum.task$env$datatypes$target
  geombar.args = cat.sum.task$geombar.args
  cat.sum = getCatSum(data, features, target, geombar.args)
  makeS3Obj("CatSumObj",
    cat.sum = cat.sum,
    task = cat.sum.task)
}

#' @export
# Print function for CatSum Object
print.CatSumObj = function(x, ...) {
  catf("Result of categorical summary for: %s", x$task$id)
  cat("\n")
  cat("Printing absolute frequency table for each categorical variable: \n")
  print(x$cat.sum$freq)
  cat("Printing relative frequency table for each categorical variable: \n")
  print(x$cat.sum$rel.freq)
  cat("Printing missing values for categorical variables: \n")
  print(x$cat.sum$nas)
  cat("Printing 2-D absolute contingency tables of categorical variables: \n")
  print(x$cat.sum$contg.list)
  cat("Printing 2-D relative contingency tables of categorical variables: \n")
  print(x$cat.sum$rel.contg.list)
  cat("Plotting ggplots for categorical variables: \n")
  warning("These are single plots")
  print(x$cat.sum$plot.list)
}
