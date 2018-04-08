#' Creates an Report object
#'
#' @param pca.obj [\code{PCAObj} Object]\cr
#'   A object of the "PCAObj" class
#' @return PCAReport
#' @examples
#' pca.task = makePCATask(id = "iris.test", data = iris, center = TRUE, target = "Species")
#' pca.result = makePCA(pca.task)
#' pca.report = makePCAReport(pca.result)
#' @import checkmate
#' @export
makePCAReport = function(pca.obj){
  assertClass(pca.obj, "PCAObj")
  report.id = reportId()

  makeS3Obj("PCAReport",
    task = pca.obj$task,
    pca.result = pca.obj,
    report.id = report.id,
    type = "PCAReport")
}

print.PCAReport = function(x, ...) {
  print(paste("report.id: ", x$report.id))
  print(x$task)
}




