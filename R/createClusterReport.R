createClusterReport = function(id, data, ...) {
  my.cluster.task = makeClusterTask(id = id, data = data,
   method = "cluster.kmeans")
  cluster.analysis = makeClusterAnalysis(my.cluster.task)
  return(makeClusterAnalysisReport(cluster.analysis))
}
