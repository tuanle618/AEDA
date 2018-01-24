#' Creates an Numeric Summary Report object
#'
#' @param num.sum.obj [\code{NumSumObj} Object]\cr
#'   A object of the "NumSumObj" class
#' @param type [\code{character(1)} Object]\cr
#'   The type of the Report to create. AS of now the default is \code{NULL}
#' @return NumSumReport
#' @import checkmate
#' @import BBmisc
#' @examples
#'  data("Boston", package = "MASS")
#'  num.sum.task = makeNumSumTask(id = "BostonTask", data = Boston, target = "medv")
#'  #get the numeric summary task object
#'  num.sum = makeNumSum(num.sum.task)
#'  #create the numeric summary report
#'  num.sum.report = makeNumSumReport(num.sum)
#' @export
makeNumSumReport = function(num.sum.obj, type = NULL){
  assertClass(num.sum.obj, "NumSumObj")
  #assertCharacter(type, min.chars = 1)

  report.id = reportId()

  makeS3Obj("NumSumReport",
    num.sum.task = num.sum.obj$task,
    num.sum.df = num.sum.obj$num.sum.df,
    num.sum.var = num.sum.obj$num.sum,
    report.id = report.id,
    type = "NumericReport")
}

print.NumSumReport = function(x, ...) {
  print(x$num.sum.task)
}
