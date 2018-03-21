#' @title Calculates the PCA
#'
#' @description
#'   Principal components analysis on the given numeric data matrix and returns the results as an object
#'
#' @param pca.task [\code{PCATask}]\cr
#'   A pcaTask Object
#'
#'
#' @return PCAObject
#'
#' @examples
#' my.task = makePCATask(id = "test", data = cars)
#' my.pca1 = makePCA(my.task)
#'
#' test.task = makePCATask(id = "Probe", data = iris, target = "Petal.Length",
#'                         tol = 1e-1, center = TRUE)
#' my.pca2 = makePCA(test.task)
#' @import checkmate
#' @import BBmisc
#' @importFrom stats prcomp
#' @export
makePCA = function(pca.task){
  assertClass(pca.task, "PCATask")
  data = pca.task$env$data
  num.cols = unique(c(pca.task$features$num, pca.task$features$int))
  selected.data = subset(data, select = num.cols)
  features = unlist(pca.task$features)
  all.args = append(list(x = selected.data), pca.task$pca.args)
  pca.Result = do.call(prcomp, all.args)

  makeS3Obj("PCAObj",
    pcaResult = pca.Result,
    task = pca.task)
}

