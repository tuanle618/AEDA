#' Creates an PCA Report object
#'
#' @param analysis.obj [\code{PCAObj} Object]\cr
#'   A object of the "PCAObj" class
#' @return PCAReport
#' @examples
#' test.task = makePCATask(id = "Probe", data = iris, center = TRUE)
#' my.pca2 = makePCA(test.task)
#' report1 = makePCAReport(my.pca2)
#' @import checkmate
#' @export
makeReport.PCAObj = function(analysis.obj){
  assertClass(analysis.obj, "PCAObj")
  report.id = reportId()
  if (analysis.obj$type == "PCAPlot") {
    plot.code = generatePCAPlot(analysis.obj, report.id)
  }
  makeS3Obj("PCAReport", analysis.obj,
   plot.code = plot.code,
   report.id = report.id)
}

print.PCAReport = function(x, ...) {
  catf("Task: %s", x$id)
  catf("Type: %2s", x$type)
  catf("Name of the Data: %s", x$data.name)
  print(x$pcaResult)
}
