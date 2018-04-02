#' Creates an PCA Report object
#'
#' @param analysis.obj [\code{PCAObj} Object]\cr
#'   A object of the "PCAObj" class
#' @return PCAReport
#' @examples
#' my.task = makePCATask(id = "test", data = cars)
#' my.pca = makePCA(my.task)
#' report = makeReport(my.pca)
#' @import checkmate
#' @export
makeReport.PCAObj = function(analysis.obj){
  assertClass(analysis.obj, "PCAObj")
  report.id = reportId()
  plot.code = generatePCAPlot(pca.obj, report.id)
  makeS3Obj2("PCAReport", analysis.obj,
   plot.code = plot.code,
   report.id = report.id)
}

print.PCAReport = function(x, ...) {
  catf("Task: %s", x$id)
  catf("Type: %2s", x$type)
  catf("Name of the Data: %s", x$data.name)
  print(x$pcaResult)
}
