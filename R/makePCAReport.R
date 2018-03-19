#' Creates an Report object
#'
#' @param pca.obj [\code{PCAObj} Object]\cr
#'   A object of the "PCAObj" class
#' @return PCAReport
#' @import checkmate
#' @export
makePCAReport = function(pca.obj){
  assertClass(pca.obj, "PCAObj")

  report.id = reportId()

  makeS3Obj("PCAReport",
    task = pca.obj$task,
    pcaResult = pca.obj$pcaResult,
    report.id = report.id,
    type = "PCAReport")
}

#' my.task = makePCATask(id = "test", data = cars)
#' my.pca = makePCA(my.task)
#' report = makePCAReport(my.pca)
#' test.task = makePCATask(id = "Probe", data = iris, target = "Petal.Length",
#'                         tol = 1e-1, center = TRUE)
#' my.pca2 = makePCA(test.task)
#' report1 = makePCAReport(my.pca2)



