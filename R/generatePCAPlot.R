#' @title Generates code for a PCA Plot
#'
#' @description
#' generates code for a PCA Plot
#' @param pca.obj [\code{PCATask}]\cr
#'   A PCATask Object
#' @param obj.name [\code{character()}]
#'   the name of object from where to get the data.
#'   make(PCA)Report function generates a random string for this.
#' @return R Code as string
#'
#' @import checkmate
generatePCAPlot = function(pca.obj, obj.name) {
  assertClass(pca.obj, "PCAObj")

  plot.PCAResult = function(x, ...) {
    autoplot(x, scale = TRUE, loadings = TRUE, loadings.colour = "blue",
      loadings.label = TRUE, ...)
}

  makeS3Obj("PlotPCA",
    code = plot.PCAResult(pca.obj$pcaResult))
}


# Intern example

#' my.task = makePCATask(id = "test", data = cars)
#' my.pca1 = makePCA(my.task)
#' generatePCAPlot(my.pca1, "test")
#'
#' test.task = makePCATask(id = "Probe", data = iris, target = "Petal.Length",
#'                         tol = 1e-1, center = TRUE)
#' my.pca2 = makePCA(test.task)
#' generatePCAPlot(my.pca2, "Probe")

