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
#' @param theme [\code{character()}]\cr
#'   This param is the theme of the YAML-header. If set to NULL no theme will
#'   be used
#' @param df.print [\code{character()}]\cr
#'   This param sets the YAML header for how Dataframed should be printed.
#'   If set to NULL df.print param will not be used.
#'
#' @return creates rmd Files, returns NULL
#'
#' @examples
#'
#' data("airquality")
#' basic.report.task = makeReportTask(id = "AirqualityTask", data = airquality, target = "Wind")
#' basic.report = makeBasicReport(basic.report.task, data = airquality)
#'
#' data("Boston", package = "MASS")
#' num.sum.task = makeNumSumTask(id = "BostonTask", data = Boston, target = "medv")
#' num.sum = makeNumSum(num.sum.task)
#' num.sum.report = makeNumSumReport(num.sum)
#'
#' my.task = makeCorrTask(id = "test", data = cars)
#' my.corr = makeCorr(my.task)
#' report1 = makeCorrReport(my.corr, type = "CorrPlot")
#
#' data(diamonds, package = "ggplot2")
#' my.task = makeCorrTask(id = "test2", data = diamonds)
#' my.corr = makeCorr(my.task)
#' report2 = makeCorrReport(my.corr, type = "CorrPlot")
#'
#' data("Arthritis", package = "vcd")
#' cat.sum.task = makeCatSumTask(id = "Arthritis.Task", data = Arthritis, target = "Improved", na.rm = TRUE)
#' cat.sum = makeCatSum(cat.sum.task)
#' cat.sum.report = makeCatSumReport(cat.sum)
#'
#' #combine all reports
#' finishReport(basic.report, num.sum.report, report1, report2, cat.sum.report)
#'
#' @import checkmate
#' @import BBmisc
#' @export
finishReport = function(..., sub.dir = "Data_Report", save.mode = TRUE, theme = "cosmo", df.print = "paged"){
  x = list(...)
  assertList(x, types = c("CorrReport", "PcaReport", "NumSumReport", "BasicReport", "CatSumReport", "ClusterAnalysisReport"))
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
  report.con = file("MainReport.rmd", "w", encoding = rmdEncoding())
  writeHeader("AEDA Report", report.con, theme = theme, df.print = df.print)

  # for now load AEDA in mainReport
  writeLines("```{r, echo=FALSE, warning=FALSE}", con = report.con)
  writeLines("devtools::load_all()", con = report.con)
  writeLines("#library(AEDA)", con = report.con)
  writeLines("```", con = report.con)

  for (i in seq.int(n)) {
    section.name = paste0(getType(x[[i]]), "_", getId(x[[i]]))
    catf("```{r %s, child = \"%s\"}", section.name, child.names[i], file = report.con)
    writeLines("```", con = report.con)
    writeLines("", con = report.con)
  }
  close(report.con)
}
