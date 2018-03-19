#' Creates a report object
#'
#' @param analysis.obj [\code{Report} Object]\cr
#'   An Analysis Object or in case of basic summary it is a basic.report.task
#' @examples
#'   data("airquality")
#'   my.basic.report.task = makeBasicReportTask(id = "test.report", data = airquality, target = "Wind")
#'   basic.report = makeReport(my.report.task, data = airquality)
#'
#'   my.creport.task = makeCorrTask(id = "corr.report", data = airquality)
#'   my.creport = makeCorr(my.creport.task)
#'   corr.report = makeReport(my.creport, type = "CorrPlot")

#' @return a report.analysis.obj
#' @export


makeReport = function(analysis.obj) UseMethod("makeReport")

makeReport.default = function(analysis.obj){
  warning(paste0("makeReport does not know how to handle object of class \"",
    class(x), "\""))
}
