#' @title Calculates a Cluster Analysis
#'
#' @description
#' makeClusterAnalysis calculates a cluster analysis and\cr
#' wrapes an object around it
#'
#' @param cluster.task [\code{ClusterTask}]\cr
#'   A ClusterTask Object
#' @return ClusterAnalysis Object
#' @examples
#'  my.cluster.task = makeClusterTask(id = "iris", data = iris,
#'   target = "Species", method = "cluster.kmeans")
#'  cluster.summary = makeClusterAnalysis(my.cluster.task)
#' @import checkmate
#' @import BBmisc
#' @importFrom cluster pam
#' @importFrom cluster diana
#' @importFrom kernlab kkmeans
#' @importFrom stats kmeans
#' @importFrom stats hclust
#' @importFrom mclust Mclust
#' @importFrom NbClust NbClust
#' @importFrom factoextra fviz_nbclust
#' @importFrom factoextra fviz_cluster
#' @importFrom factoextra get_dist
#' @importFrom factoextra fviz_dist
#' @importFrom factoextra eclust
#' @importFrom factoextra fviz_dend
#' @importFrom factoextra fviz_silhouette
#' @importFrom stats prcomp
#' @importFrom factoextra fviz_mclust
#' @importFrom dbscan dbscan
#' @import factoextra
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
    task = cluster.task,
    cluster.analysis = cluster.analysis
  )
}

#' @export
# Print function for ClusterAnalysis Object
print.ClusterAnalysisObj = function(x, ...) {
  print(x$task)
  print(x$cluster.analysis$cluster.all$cluster.plot)
}
