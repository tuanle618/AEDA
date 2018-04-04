#' @title Creates a Cluster Report Object
#' @param id [\code{character(1)}]\cr
#'   ID of the Task Object. \code{\link{makeClusterTask}}
#' @param data [\code{data.frame}]\cr
#'   A Dataframe with different variables
#' @param target [\code{character(1)}]\cr
#'   Target column. If not available please insert as \code{NULL}.
#' @param ...
#'   For now it does nothing
#'
#' @export
createClusterReport = function(id, data, ...) {
  my.cluster.task = makeClusterTask(id = id, data = data,
   method = "cluster.kmeans")
  cluster.analysis = makeClusterAnalysis(my.cluster.task)
  return(makeClusterAnalysisReport(cluster.analysis))
}
