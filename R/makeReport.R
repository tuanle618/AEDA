#' Creates a report object
#'
#' @param analysis.obj [\code{Report} Object]\cr
#'   An Analysis Object or in case of basic summary it is a basic.report.task
#' @examples
#'   data("airquality")
#'   basic.report.task = makeBasicReportTask(id = "test.report", data = airquality, target = "Wind")
#'   basic.report.result = makeReport(basic.report.task)
#'
#'   corr.report.task = makeCorrTask(id = "corr.report", data = airquality)
#'   corr.report.result = makeCorr(corr.report.task)
#'   corr.report = makeReport(corr.report.result)

#' @return a report.analysis.obj
#' @export


makeReport = function(analysis.obj) UseMethod("makeReport")

makeReport.default = function(analysis.obj){
  warning(paste0("makeReport does not know how to handle object of class \"",
    class(x), "\""))
}
