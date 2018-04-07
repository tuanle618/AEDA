#' @title Calculates the PCA
#'
#' @description
#'   Principal components analysis on the given numeric data matrix and returns the results as an object
#'
#' @param pca.task [\code{PCATask}]\cr
#'   A pcaTask Object
#'
#' @return PCAObject
#'
#' @examples
#' pca.task = makePCATask(id = "iris.test", data = iris, target = "Species",
#'                         tol = 1e-1, center = TRUE)
#' pca.result = makePCA(pca.task)
#' @import checkmate
#' @import BBmisc
#' @importFrom stats prcomp
#' @importFrom factoextra fviz_eig
#' @importFrom factoextra fviz_pca_ind
#' @import ggplot2
#' @import ggfortify
#' @export
makePCA = function(pca.task){
  assertClass(pca.task, "PCATask")
  data = pca.task$env$data
  num.cols = pca.task$features
  selected.data = subset(data, select = num.cols)
  all.args = append(list(x = selected.data), pca.task$pca.args)
  pca.result = do.call(prcomp, all.args)

  ###include plots:
  #scree
  pca.scree = fviz_eig(pca.result, addlabels = TRUE) + theme_gray()
  #scatter
  pca.scatter.1 = autoplot(pca.result, scale = TRUE, loadings = TRUE, loadings.colour = "blue",
    loadings.label = TRUE, color = "class", main = "Scatterplot PCA")  + theme_gray()
  pca.scatter.named = autoplot(pca.result, scale = TRUE, shape = FALSE, label.size = 3, loadings = TRUE,
    loadings.colour = "blue", loadings.label = TRUE, main = "Scatterplot PCA with rownames")  + theme_gray()
  #fviz extras
  pca.scatter.indidivuals = fviz_pca_ind(pca.result,
    col.ind = "cos2",
    gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE) + theme_gray()

  plotlist = list(pca.scree = pca.scree, pca.scatter.1 = pca.scatter.1,
    pca.scatter.named = pca.scatter.named, pca.scatter.indidivuals = pca.scatter.indidivuals)

  makeS3Obj("PCAObj",
    pca.result = pca.result,
    plotlist = plotlist,
    task = pca.task)
}

#' @export
# Print function for PCA Object
print.PCAObj = function(x, ...) {
  print(x$task)
  catf("Result of principal component analysis for: %s", x$task$id)
  cat("\n")
  cat("Rotation matrix:\n")
  print(head(x$pca.result$rotation))
  print(x$plotlist$pca.scree)
  print(x$plotlist$pca.scatter.1)
}

