#' Writes a rmd file Report [WIP]
#'
#' @param report [\code{Report} Object]\cr
#'   A report Object
#' @param ... [\code{character(1)}]\cr
#'   for now its not used
#' @param sub.dir [\code{character(1)}]\cr
#'   the name of the (relative) sub-directory where the data report will be saved. Default is \code{Data_Report}
#' @param save.mode [\code{logical(1)}]\cr
#'   In Save mode its not possible to use an existing folder.
#'   To ensure no data is lost, a new folder will be created (if possible).
#'   Default is \code{TRUE}
#' @param override [\code{logical(1)}]\cr
#'   override controls if the function is allowed to override
#'   an existing rmd-file
#' @examples
#'   data("airquality")
#'   my.report.task = makeBasicReportTask(id = "test.report", data = airquality, target = "Wind")
#'   basic.report = makeBasicReport(my.report.task)
#'   writeReport(basic.report, save.mode = FALSE, override = TRUE)
#'
#'   my.creport.task = makeCorrTask(id = "corr.report", data = airquality)
#'   my.creport = makeCorr(my.creport.task)
#'   corr.report = makeCorrReport(my.creport, type = "CorrPlot")
#'   writeReport(corr.report, save.mode = FALSE, override = TRUE)
#'
#'   # Or put the reports together
#'   finishReport(basic.report, corr.report, save.mode = FALSE, override = TRUE)
#'
#'
#' @return Invisible NULL; creates a rmd report file
#' @export


writeReport = function(report, sub.dir, save.mode, override) UseMethod("writeReport")

writeReport.default = function(report, sub.dir = NULL, save.mode = NULL){
  warning(paste0("writeReport does not know how to handle object of class \"",
    class(report), "\""))
}
