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
  cat.sum = getCatSum(data, features, target)

  makeS3Obj("CatSumObj",
    cat.sum = cat.sum,
    task = cat.sum.task)
}

#' @export
# Print function for CatSum Object
print.CatSumObj = function(x, ...) {
  catf("Result of categorical summary for: %s", x$task$id)
  cat("\n")
  print(x$cat.sum)
}
