#' Creates an Correlation Report object
#'
#' @param analysis.obj [\code{CorrObj} Object]\cr
#'   A object of the "CorrObj" class
#' @return CorrReport
#' @examples
#' corr.task = makeCorrTask(id = "test", data = cars)
#' corr.result = makeCorr(corr.task)
#' corr.report = makeReport(corr.result)
#' @import checkmate
#' @export
makeReport.CorrObj = function(analysis.obj){
  assertClass(analysis.obj, "CorrObj")
  report.id = reportId()
  if (analysis.obj$type == "CorrPlot") {
    plot.code = generateCorrPlot(analysis.obj, report.id)
  }
  makeS3Obj2("CorrReport", analysis.obj,
    plot.code = plot.code,
    report.id = report.id)
}

print.CorrReport = function(x, ...) {
  catf("Task: %s", x$id)
  catf("Type: %2s", x$type)
  catf("Name of the Data: %s", x$data.name)
  cat("Correlationmatrix: \n")
  print(x$corr.matrix)
}

