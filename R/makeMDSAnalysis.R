#' @title Calculates a Multidimensional Scaling
#'
#' @description
#' makeMDS calculates a multidimensional scaling and\cr
#' wrapes an object around it
#'
#' @param mds.task [\code{MDSTask}]\cr
#'   A MDSTask Object
#' @return MDS Object
#' @examples
#' my.mds.task = makeMDSTask(id = "swiss", data = swiss)
#' mds.analysis = makeMDSAnalysis(my.mds.task)
#' @import checkmate
#' @import BBmisc
#' @importFrom stats cmdscale
#' @export
makeMDSAnalysis = function(mds.task){
  assertClass(mds.task, "MDSTask")

  data = mds.task$env$data
  num.features = unlist(mds.task$numdatatypes)
  method = mds.task$method
  par.vals = mds.task$par.vals
  dist = mds.task$dist
  mds.analysis = getMDSAnalysis(dist, method, par.vals)

  makeS3Obj("MDSAnalysisObj",
    task = mds.task,
    mds.analysis = mds.analysis
  )
}

#' @export
# Print function for ClusterAnalysis Object
print.MDSAnalysisObj = function(x, ...) {
  print(x$task)
  print(x$mds.analysis$mds.plot)
}
