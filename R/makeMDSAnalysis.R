#' @title Calculates a Multidimensional Scaling
#'
#' @description
#' makeMDS calculates a multidimensional scaling and\cr
#' wrapes an object around it
#'
#' @param mds.task [\code{ClusterTask}]\cr
#'   A MDSTask Object
#' @return MDS Object
#' @examples
#' my.mds.task = makeMDSTask(id = "swiss", data = swiss,
#' target = "Infant.Mortality")
#' mds.analysis = makeMDSAnalysis(my.mds.task)
#' @import checkmate
#' @import BBmisc
#' @importFrom stats cmdscale
#' @export
makeMDSAnalysis = function(mds.task){
  assertClass(mds.task, "MDSTask")

  data = mds.task$env$data
  num.features = unlist(mds.task$numdatatypes)
  target = mds.task$env$datatypes$target
  method = mds.task$method
  par.vals = mds.task$par.vals
  dist = mds.task$dist
  mds.analysis = getMDSAnalysis(dist, method, par.vals)

  makeS3Obj("MDSAnalyisObj",
    task = mds.task,
    mds.analysis = mds.analysis
  )
}

#' @export
# Print function for ClusterAnalysis Object
print.MDSAnalyisObj = function(x, ...) {
  print(x$task)
  print(x$mds.analysis$mds.plot)
}
