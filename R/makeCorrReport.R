#' Creates an Report object
#'
#' @param corr.obj [\code{CorrObj} Object]\cr
#'   A object of the "CorrObj" class
#' @param type [\code{character(1)} Object]\cr
#'   The type of the Report to create. Example: "CorrPlot"
#' @return CorrReport
#' @import checkmate
#' @export
makeCorrReport = function(corr.obj, type){
  assertClass(corr.obj, "CorrObj")
  assertCharacter(type, min.chars = 1)

  report.id = reportId()
  if (type == "CorrPlot"){
    plot.code = generateCorrPlot(corr.obj, report.id)
  }
  makeS3Obj2("CorrReport", corr.obj,
    plot.code = plot.code,
    report.id = report.id)
}

# my.task = makeCorrTask(id = "test", data = cars)
# my.corr = makeCorr(my.task)
# report1 = makeCorrReport(my.corr, type = "CorrPlot")
