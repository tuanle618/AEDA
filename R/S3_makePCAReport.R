#' Creates an PCA Report object
#'
#' @param analysis.obj [\code{PCAObj} Object]\cr
#'   A object of the "PCAObj" class
#' @return PCAReport
#' @examples
#' pca.task = makePCATask(id = "iris.test", data = iris, center = TRUE, target = "Species")
#' pca.result = makePCA(pca.task)
#' pca.report = makePCAReport(pca.result)
#' @import checkmate
#' @export
makeReport.PCAObj = function(analysis.obj){
  assertClass(analysis.obj, "PCAObj")
  report.id = reportId()

  makeS3Obj("PCAReport",
   task = analysis.obj$task,
   pca.result = analysis.obj,
   report.id = report.id,
   type = "PCAReport")
}

print.PCAReport = function(x, ...) {
  print(paste("report.id: ", x$report.id))
  print(x$task)
}
