#' @title Creates a BasicReport Object [WIP]
#'
#' @description
#' A Basic Report Object will be calculated containing the report task, a basic data summary and missing value summary
#'
#' @param analysis.obj [\code{ReportTask}]\cr
#'   A Report Task object containing general information of the data analysis problem
#' @return [\code{BasicReport}]
#' @import checkmate
#' @import BBmisc
#' @examples
#'  data("airquality")
#'  basic.report.task = makeBasicReportTask(id = "test.report", data = airquality, target = "Wind")
#'  basic.report.result = makeReport(basic.report.task)
#'
#' @export
makeReport.BasicReportTask = function(analysis.obj){
  assertClass(analysis.obj, "BasicReportTask")
  #report.id = reportId()
  report.id = deparse(substitute(analysis.obj))
  basic.data.summary = basicDataSummary(data = analysis.obj$env$data, target = analysis.obj$env$target)
  na.summary = naSummary(data = analysis.obj$env$data, dataset.name = analysis.obj$dataset.name)

  makeS3Obj("BasicReport",
    task = analysis.obj,
    basic.data.summary = basic.data.summary,
    na.summary = na.summary,
    report.id = report.id,
    type = "BasicReport")
}

print.BasicReport = function(x, ...) {
  print(x$report.task)
}
