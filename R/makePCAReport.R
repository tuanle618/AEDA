#' Creates an Report object
#'
#' @param pca.obj [\code{PCAObj} Object]\cr
#'   A object of the "PCAObj" class
#' @return PCAReport
#' @examples
#' test.task = makePCATask(id = "Probe", data = iris, center = TRUE)
#' my.pca2 = makePCA(test.task)
#' report1 = makePCAReport(my.pca2)
#' @import checkmate
#' @export
makePCAReport = function(pca.obj){
  assertClass(pca.obj, "PCAObj")
  report.id = reportId()
  plot.code = generatePCAPlot(pca.obj, report.id)

  makeS3Obj("PCAReport",
    task = pca.obj$task,
    pcaResult = pca.obj$pcaResult,
    report.id = report.id,
    plot.code = plot.code)
}



