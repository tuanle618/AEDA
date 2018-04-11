#' Creates an Multidimensional Scaling Report Object
#'
#' @param analysis.obj [\code{MDSAnalysisObj} Object]\cr
#'   A object of the "MDSAnalyisObj" class
#' @return A MDSAnalysisReport Object
#' @import checkmate
#' @import BBmisc
#' @examples
#'  mds.task = makeMDSTask(id = "swiss", data = swiss)
#'  mds.analysis.result = makeMDSAnalysis(mds.task)
#'  mds.analysis.report = makeReport(mds.analysis.result)
#' @export
makeReport.MDSAnalysisObj = function(analysis.obj){
  assertClass(analysis.obj, "MDSAnalysisObj")

  report.id = reportId()

  makeS3Obj("MDSAnalysisReport",
    task = analysis.obj$task,
    mds.analysis = analysis.obj$mds.analysis,
    report.id = report.id,
    type = "MSDAnalysisReport")
}

print.MDSAnalysisReport = function(x, ...) {
  print(x$task)
}
