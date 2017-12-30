#' Writes a rmd file for the Basic Report [WIP]
#'
#' @param basic.report [\code{BasicReport} Object]\cr
#'   The report Object which should be converted to an rmd file
#' @param sub.dir [\code{character(1)}]\cr
#'   the name of the (relative) sub-directory where the data report will be saved. Default is \code{Data_Report}
#' @param save.mode [\code{logical(1)}]\cr
#'   In Save mode its not possible to use an existing folder.
#'   To ensure no data is lost, a new folder will be created (if possible).
#'   Default is \code{TRUE}
#' @examples
#'   data("airquality")
#'   my.report.task = makeReportTask(id = "test.report", data = airquality, target = "Wind")
#'   basic.report = makeBasicReport(my.report.task, data = airquality)
#'   writeBasicReport(basic.report)
#'
#'   data("iris")
#'   my.report.task2 = makeReportTask(id = "test.report2", data = iris, target = "Species")
#'   basic.report2 = makeBasicReport(my.report.task2, data = iris)
#'   writeBasicReport(basic.report2)
#' @return Invisible NULL
#' @import checkmate
#' @export
writeBasicReport = function(basic.report, sub.dir = "Data_Report", save.mode = TRUE){
  assertClass(basic.report, "BasicReport")
  assertCharacter(sub.dir, len = 1L, min.chars = 1L)
  assertLogical(save.mode, len = 1L)
  # Create sub directory, save current wd and set new wd to the new directory
  origin.wd = createDir(sub.dir, save.mode)

  # TryCatch sets wd back and closes all open connections if an error occurs
  tryCatch({
    ##try part:
    #start the report file
    report.con = file(paste0("basicReport_", basic.report$report.task$dataset.name, ".rmd"), "w") #or include task.id ?
    writeLines("## Basic Report from AEDA containing a basic and missing values summary", con  = report.con)
    writeLines("```{r}", con = report.con)

    #save data
    if (save.mode) {
      file.name = paste0(basic.report$report.task$dataset.name, ".rds")
      saveRDS(basic.report$report.task$env$data, file = file.name)
    }
    # load data
    data.path = file.path(".", basic.report$report.task$dataset.name)
    rmdloadData(basic.report$report.task$dataset.name, data.path, report.con)

    # save object and write code to load it in the rmd-file
    saveLoadObj(basic.report, deparse(substitute(basic.report)), report.con)

    writeLines("```", con = report.con)

    writeLines("```{r}", con = report.con)
    #testing:
    #vec = c("5+5", "a = TRUE", "print('Hallo')")
    #rmdWriteLines(vec = vec,  con = report.con)
    writeLines("# Declaring object for more convenience and clarity:", con = report.con)
    writeLines(paste0("basic.report.obj = ", basic.report$report.id), con = report.con)
    writeLines("```", con = report.con)

    writeLines("Some text; Basic Summary ....", con = report.con)
    writeLines("```{r}", con = report.con)

    writeLines("```", con = report.con)

  }, finally = {
    setwd(origin.wd)
    close(report.con)
  })
  return(invisible(NULL))
}
