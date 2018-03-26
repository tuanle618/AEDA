#' Creates an Cluster Analysis Report object
#'
#' @param analysis.obj [\code{ClusterAnalysisObj} Object]\cr
#'   A object of the "ClusterAnalysisObj" class
#' @return ClusterAnalysisObj
#' @import checkmate
#' @import BBmisc
#' @examples
#'  my.cluster.task = makeClusterTask(id = "iris", data = iris,
#'   target = "Species", method = "cluster.kmeans")
#'  cluster.analysis = makeClusterAnalysis(my.cluster.task)
#'  cluster.report = makeReport(cluster.analysis)
#' @export
makeReport.ClusterAnalysisObj = function(analysis.obj){
  assertClass(analysis.obj, "ClusterAnalysisObj")

  report.id = reportId()

  makeS3Obj("ClusterAnalysisReport",
    report.task = analysis.obj$task,
    cluster.analysis = analysis.obj$cluster.analysis,
    report.id = report.id,
    type = "ClusterAnalysisReport")
}

print.ClusterAnalysisReport = function(x, ...) {
  print(x$task)
}
