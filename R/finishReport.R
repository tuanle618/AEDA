#' @title Creates the main Report with childs
#'
#' @description
#' The function writes the MainReport rmd file and write the child rmd files and
#' organize them together in one report
#'
#' @param ... \cr
#'   Report objects
#' @param sub.dir [\code{path}]\cr
#'   The path where the child rmd files will be created
#' @param save.mode [\code{logical(1)}]\cr
#'   In Save mode its not possible to use an existing folder.
#'   To ensure no data is lost, a new folder will be created (if possible).
#'
#' @return creates rmd Files, returns NULL
#'
#' @examples
#'
#' #' data("airquality")
#' basic.report.task = makeReportTask(id = "AirqualityTask", data = airquality, target = "Wind")
#' basic.report = makeBasicReport(basic.report.task, data = airquality)
#'
#' data("Boston", package = "MASS")
#' num.sum.task = makeNumSumTask(id = "BostonTask", data = Boston, target = "medv")
#' num.sum = makeNumSum(num.sum.task)
#' num.sum.report = makeNumSumReport(num.sum)
#'
#' finishReport(basic.report, num.sum.report)
#'
#' my.task = makeCorrTask(id = "test", data = cars)
#' my.corr = makeCorr(my.task)
#' report1 = makeCorrReport(my.corr, type = "CorrPlot")
#'
#' library(ggplot2)
#' data(diamonds, package = "ggplot2")
#' my.task = makeCorrTask(id = "test2", data = diamonds)
#' my.corr = makeCorr(my.task)
#' report2 = makeCorrReport(my.corr, type = "CorrPlot")
#'
#' finishReport(report1, report2)
#'
#' @import checkmate
#' @import BBmisc
#' @export
finishReport = function(..., sub.dir = "Data_Report", save.mode = TRUE){
  x = list(...)
  assertList(x, types = c("CorrReport", "PcaReport", "NumSumReport", "BasicReport"))
  assertLogical(save.mode)
  assert_path_for_output(sub.dir, overwrite = !save.mode)

  n = length(x)
  child.names = vector(mode = "character", length = n)
  # Genrate Reports
  for (i in seq.int(n)) {
    ### writeReport is the S3 Method which should pick the correct write function for each object
    child.names[i] = writeReport(x[[i]], sub.dir, save.mode = FALSE)
  }
  # Organize Childs
  report.con = file("MainReport.rmd", "w")
  for (i in seq.int(n)) {
    section.name = paste0(getType(x[[i]]), "_", getId(x[[i]]))
    catf("```{r %s, child = \"%s\"}", section.name, child.names[i], file = report.con)
    writeLines("```", con = report.con)
    writeLines("", con = report.con)
  }
  close(report.con)
}
