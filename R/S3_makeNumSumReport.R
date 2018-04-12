#' Creates an Numeric Summary Report object
#'
#' @param analysis.obj [\code{NumSumObj} Object]\cr
#'   A object of the "NumSumObj" class
#' @return NumSumReport
#' @import checkmate
#' @import BBmisc
#' @examples
#'  data("Aids2", package = "MASS")
#'  num.sum.task = makeNumSumTask(id = "AidsTask", data = Aids2,
#'    target = "sex")
#'  #get the numeric summary task object
#'  num.sum.result = makeNumSum(num.sum.task)
#'  #create the numeric summary report
#'  num.sum.report = makeReport(num.sum.result)
#' @export
makeReport.NumSumObj = function(analysis.obj){
  assertClass(analysis.obj, "NumSumObj")
  #assertCharacter(type, min.chars = 1)

  #report.id = reportId()
  report.id = deparse(substitute(analysis.obj))

  makeS3Obj("NumSumReport",
    num.sum.task = analysis.obj$task,
    num.sum.df = analysis.obj$num.sum.df,
    num.sum.var = analysis.obj$num.sum,
    report.id = report.id,
    type = "NumericReport")
}

print.NumSumReport = function(x, ...) {
  print(x$num.sum.task)
}
