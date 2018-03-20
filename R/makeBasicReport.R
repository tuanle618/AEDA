#' @title Creates a BasicReport Object [WIP]
#'
#' @description
#' A Basic Report Object will be calculated containing the report task, a basic data summary and missing value summary
#'
#' @param basic.report.task [\code{BasicReportTask}]\cr
#'   A Basic Report Task object containing general information of the data analysis problem
#'
#' @return [\code{BasicReport}]\cr
#' @import checkmate
#' @import BBmisc
#' @examples
#'  data("airquality")
#'  my.basic.report.task = makeBasicReportTask(id = "test.report", data = airquality, target = "Wind")
#'  basic.report = makeBasicReport(my.basic.report.task)
#'
#' @export
makeBasicReport = function(basic.report.task){
  assertClass(basic.report.task, "BasicReportTask")
  report.id = reportId()
  basic.data.summary = basicDataSummary(data = basic.report.task$env$data, target = basic.report.task$env$target)
  na.summary = naSummary(data = basic.report.task$env$data, dataset.name = basic.report.task$dataset.name)

  makeS3Obj("BasicReport",
    task = basic.report.task,
    basic.data.summary = basic.data.summary,
    na.summary = na.summary,
    report.id = report.id,
    type = "BasicReport")
}

print.BasicReport = function(x, ...) {
  print(x$report.task)
}
