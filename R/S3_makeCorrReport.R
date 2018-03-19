#' Creates an Correlation Report object
#'
#' @param corr.obj [\code{CorrObj} Object]\cr
#'   A object of the "CorrObj" class
#' @return CorrReport
#' @examples
#' my.task = makeCorrTask(id = "test", data = cars)
#' my.corr = makeCorr(my.task)
#' report = makeReport(my.corr)
#' @import checkmate
#' @export
makeReport.CorrObj = function(corr.obj){
  assertClass(corr.obj, "CorrObj")
  report.id = reportId()
  if (corr.obj$type == "CorrPlot") {
    plot.code = generateCorrPlot(corr.obj, report.id)
  }
  makeS3Obj2("CorrReport", corr.obj,
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
