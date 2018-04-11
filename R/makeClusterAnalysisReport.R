#' Creates an Cluster Analysis Report object
#'
#' @param cluster.analysis.obj [\code{ClusterAnalysisObj} Object]\cr
#'   A object of the "ClusterAnalysisObj" class
#' @return A ClusterAnalysisReport Object
#' @import checkmate
#' @import BBmisc
#' @examples
#' \dontrun{
#'  cluster.task = makeClusterTask(id = "iris", data = iris,
#'    method = "cluster.kmeans")
#'  cluster.analysis.result = makeClusterAnalysis(cluster.task)
#'  cluster.analysis.report = makeClusterAnalysisReport(cluster.analysis.result)
#' }
#' @export
makeClusterAnalysisReport = function(cluster.analysis.obj){
  assertClass(cluster.analysis.obj, "ClusterAnalysisObj")

  report.id = reportId()

  makeS3Obj("ClusterAnalysisReport",
    report.task = cluster.analysis.obj$task,
    cluster.analysis = cluster.analysis.obj$cluster.analysis,
    report.id = report.id,
    type = "ClusterAnalysisReport")
}

print.ClusterAnalysisReport = function(x, ...) {
  print(x$report.task)
}
