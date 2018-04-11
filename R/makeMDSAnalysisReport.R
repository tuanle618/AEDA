#' Creates an Multidimensional Scaling Report Object
#'
#' @param mds.analysis.obj [\code{MDSAnalysisObj} Object]\cr
#'   A object of the "MDSAnalysisObj" class
#' @return A MDSAnalysisReport Object
#' @import checkmate
#' @import BBmisc
#' @examples
#'  mds.task = makeMDSTask(id = "swiss", data = swiss)
#'  mds.analysis.result = makeMDSAnalysis(mds.task)
#'  mds.analysis.report = makeReport(mds.analysis.result)
#' @export
makeMDSAnalysisReport = function(mds.analysis.obj){
  assertClass(mds.analysis.obj, "MDSAnalysisObj")

  report.id = reportId()

  makeS3Obj("MDSAnalysisReport",
    task = mds.analysis.obj$task,
    mds.analysis = mds.analysis.obj$mds.analysis,
    report.id = report.id,
    type = "MDSAnalysisReport")
}

print.MDSAnalysisReport = function(x, ...) {
  print(x$task)
}
