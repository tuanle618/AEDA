#' Writes a rmd file Report [WIP]
#'
#' @param report [\code{Report} Object]\cr
#'   A report Object
#' @param ... [\code{character(1)}]\cr
#'   for now its not used
#' @examples
#'   data("airquality")
#'   my.report.task = makeReportTask(id = "test.report", data = airquality, target = "Wind")
#'   basic.report = makeBasicReport(my.report.task, data = airquality)
#'   writeReport(basic.report)
#'
#'   my.creport.task = makeCorrTask(id = "corr.report", data = airquality)
#'   my.creport = makeCorr(my.creport.task)
#'   corr.report = makeCorrReport(my.creport, type = "CorrPlot")
#'   writeReport(corr.report)
#'
#'   # Or put the reports together
#'   finishReport(basic.report, corr.report)
#'
#'
#' @return Invisible NULL; creates a rmd report file
#' @export


writeReport = function(report, sub.dir, save.mode) UseMethod("writeReport")

writeReport.default = function(report, ...){
  warning(paste0("writeReport does not know how to handle object of class \"",
    class(x), "\""))
}
