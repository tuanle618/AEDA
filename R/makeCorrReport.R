#' Creates an Report object
#'
#' @param corr.obj [\code{CorrObj} Object]\cr
#'   A object of the "CorrObj" class
#' @return CorrReport
#' @examples
#' corr.task = makeCorrTask(id = "test", data = cars)
#' corr.result = makeCorr(corr.task)
#' corr.report = makeCorrReport(corr.result)
#' @import checkmate
#' @export
makeCorrReport = function(corr.obj){
  assertClass(corr.obj, "CorrObj")
  report.id = reportId()
  if (corr.obj$type == "CorrPlot") {
    plot.code = generateCorrPlot(corr.obj, report.id)
  }
  makeS3Obj2("CorrReport", corr.obj,
    plot.code = plot.code,
    report.id = report.id)
}


