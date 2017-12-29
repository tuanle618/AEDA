#' @title Creates a BasicReport Object [WIP]
#'
#' @description
#' A Basic Report object containing the report task, a basic data summary and missing value summary
#'
#' @param report.task [\code{ReportTask}]\cr
#'   A Report Task object containing general information of the data analysis problem
#' @param data [\code{data.frame}]\cr
#'   A Dataframe with different variables
#'
#' @return CorrTask
#'
#' @examples
#' my.task = makeCorrTask(id = "test", data = cars)
#' # Extract Data
#' my.task$env$data
#' @import checkmate
#' @import BBmisc
#' @examples
#'  data("airquality")
#'  my.report.task = makeReportTask(id = "test.report", data = airquality, target = "Wind")
#'  basic.report = makeBasicReport(my.report.task, data = airquality)
#'
#' @export
makeBasicReport = function(report.task, data){
  assertClass(report.task, "ReportTask")
  assertDataFrame(data)
  report.id = reportId()
  basic.data.summary = basicDataSummary(data = report.task$env$data, target = report.task$env$target)
  na.summary = naSummary(data = data, report.task = my.report.task)

  makeS3Obj("BasicReport",
    report.task = report.task,
    basic.data.summary = basic.data.summary,
    na.summary = na.summary,
    report.id = report.id)
}

print.BasicReport = function(x, ...) {
  print(x$report.task)
}
