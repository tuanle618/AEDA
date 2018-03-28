#' @title Computes the multidimensional scaling and plots
#'
#' @description
#'  getMDSAnalysis computes the MDS analysis of a distance matrix
#'
#' @param dist [\code{dist}]\cr
#'   A distance matrix
#' @param method [\code{character(1L)}]\cr
#'   Method used for multidimensional scaling
#' @param par.vals [\code{list()}]\cr
#'   Other arguments to be passed to selected method
#' @return [\code{list()}]
#'   A list containing the MDS analysis and plot
#' @import checkmate
#' @import BBmisc
#' @importFrom stats cmdscale
#' @export
getMDSAnalysis = function(dist, method, par.vals) {
  #define mds.method
  if (method == "cmdscale") {
    mds.method = cmdscale
    title = "Classical Metric Multidimensional Scaling"
  } else if (method == "wcmdscale") {
    mds.method = wcmdscale
    title = "Weighted Classical Metric Multidimensional Scaling"
  } else if (method == "smacofSym") {
    mds.method = smacofSym
    title = "Metric Multidimensional scaling on a symmetric dissimilarity matrix using SMACOF"
  } else if (method == "isoMDS") {
    mds.method = isoMDS
    title = "Kruskal's Non-metric Multidimensional Scaling"
  } else if (method == "sammon") {
    mds.method = sammon
    title = "Sammon's Non-metric Multidimensional Scaling"
  }

  #Compute MDS
  if (!(method == "smacoSym")) {
    mds.result = do.call(mds.method, append(list(d = dist), par.vals))
  } else {
    mds.result = do.call(mds.method, append(list(delta = dist), par.vals))
  }

  #Plot MDS
  if (method == "cmdscale" | method == "wcmdscale") {
    mds.result.data = as.data.frame(mds.result, rownames = TRUE)
  } else if (method == "smacofSym") {
    mds.result.data = as.data.frame(mds.result$conf, rownames = TRUE)
  } else if (method == "isoMDS" | method == "sammon") {
    mds.result.data = as.data.frame(mds.result$points, rownames = TRUE)
  }
  colnames(mds.result.data) = c("Dim1", "Dim2")
  # Plot MDS
  mds.plot = ggscatter(mds.result.data, x = "Dim1", y = "Dim2",
    label = rownames(mds.result.data),
    size = 1,
    repel = TRUE) + theme_classic() + ggtitle(title)

  out.list = list(mds.result = mds.result, mds.plot = mds.plot)
  return(out.list)
}
