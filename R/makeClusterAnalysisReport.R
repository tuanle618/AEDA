#' Creates an Cluster Analysis Report object
#'
#' @param cluster.analysis.obj [\code{ClusterAnalysisObj} Object]\cr
#'   A object of the "ClusterAnalysisObj" class
#' @return ClusterAnalysisObj
#' @import checkmate
#' @import BBmisc
#' @examples
#'  my.cluster.task = makeClusterTask(id = "iris", data = iris, target = "Species", method = "cluster.kmeans")
#'  cluster.analysis = makeClusterAnalysis(my.cluster.task)
#'  cluster.report = makeClusterAnalysisReport(cluster.analysis)
#' @export
makeClusterAnalysisReport = function(cluster.analysis.obj){
  assertClass(cluster.analysis, "ClusterAnalysisObj")

  report.id = reportId()

  makeS3Obj("ClusterAnalysisReport",
    task = cluster.analysis$task,
    cluster.analysis = cluster.analysis$cluster.analysis,
    report.id = report.id,
    type = "ClusterAnalysisReport")
}

print.ClusterAnalysisReport = function(x, ...) {
  print(x$task)
}
