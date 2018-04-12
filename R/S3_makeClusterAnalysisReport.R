#' Creates an Cluster Analysis Report object
#'
#' @param analysis.obj [\code{ClusterAnalysisObj} Object]\cr
#'   A object of the "ClusterAnalysisObj" class
#' @return A ClusterAnalysisReport Object
#' @import checkmate
#' @import BBmisc
#' @examples
#' \dontrun{
#'  cluster.task = makeClusterTask(id = "iris", data = iris,
#'   method = "cluster.kmeans")
#'  cluster.analysis.result = makeClusterAnalysis(cluster.task)
#'  cluster.analysis.report = makeReport(cluster.analysis.result)
#' }
#' @export
makeReport.ClusterAnalysisObj = function(analysis.obj){
  assertClass(analysis.obj, "ClusterAnalysisObj")

  #report.id = reportId()
  report.id = deparse(substitute(analysis.obj))

  makeS3Obj("ClusterAnalysisReport",
    task = analysis.obj$task,
    cluster.analysis = analysis.obj$cluster.analysis,
    report.id = report.id,
    type = "ClusterAnalysisReport")
}

print.ClusterAnalysisReport = function(x, ...) {
  print(x$task)
}
