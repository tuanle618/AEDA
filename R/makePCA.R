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
#' iris<-iris[,-5] # 'x' must be numeric
#' test.task = makePCATask(id = "Probe", data = iris, target = "Petal.Lenght",
#'             scale = TRUE, tol = 1e-1)
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
  all.args = append(list(x = selected.data, scale = pca.task$scale), pca.task$pca.args)
  pcaResult = do.call(prcomp, all.args)

  makeS3Obj("PCAObj",
    pcaResult = pcaResult,
    task = pca.task)
}



