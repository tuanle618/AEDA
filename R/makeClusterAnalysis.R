#' @title Calculates a Cluster Analysis
#'
#' @description
#' makeClusterAnalysis calculates a cluster analysis and wrapes an object around it
#'
#' @param cluster.task [\code{ClusterTask}]\cr
#'   A ClusterTask Object
#' @return ClusterAnalysis Object
#' @examples
#'  my.cluster.task = makeClusterTask(id = "iris", data = iris, target = "Species", method = "cluster.kmeans")
#'  cluster.summary = makeClusterAnalysis(my.cluster.task)
#' @import checkmate
#' @import BBmisc
#' @import stats
#' @import cluster
#' @import kernlab
#' @import mclust
#' @import NbClust
#' @import factoextra
#' @import fpc
#' @import dbscan
#' @export
makeClusterAnalysis = function(cluster.task){
  assertClass(cluster.task, "ClusterTask")

  data = cluster.task$env$data
  num.features = unlist(cluster.task$numdatatypes)
  target = cluster.task$env$datatypes$target
  method = cluster.task$method
  par.vals = cluster.task$par.vals
  random.seed = cluster.task$random.seed
  scale.num.data = cluster.task$scale.num.data
  cluster.analysis = getClusterAnalysis(data, num.features, method, par.vals, random.seed, scale.num.data)

  makeS3Obj("ClusterAnalysisObj",
    cluster.task = cluster.task,
    cluster.analysis = cluster.analysis
  )
}

#' @export
# Print function for ClusterAnalysis Object
print.ClusterAnalysisObj = function(x, ...) {
  print(x$cluster.task)
  #add more...:
}
