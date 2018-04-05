#' Writes a rmd file for the Basic Report [WIP]
#'
#' @param report [\code{BasicReport} Object]\cr
#'   The report Object which should be converted to an rmd file
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
#'   report = makeReport(my.report.task)
#'   writeReport(report, save.mode = FALSE, override = TRUE)
#'
#'   data("iris")
#'   my.report.task2 = makeBasicReportTask(id = "test.report2", data = iris, target = "Species")
#'   report2 = makeReport(my.report.task2)
#'   writeReport(report2, save.mode = FALSE, override = TRUE)
#' @return Invisible NULL
#' @import checkmate
#' @importFrom BBmisc catf
#' @export
writeReport.BasicReport = function(report, sub.dir = "Data_Report", save.mode = TRUE, override = FALSE){
  report.env = new.env(parent = .GlobalEnv)
  assertClass(report, "BasicReport")
  assertCharacter(sub.dir, len = 1L, min.chars = 1L)
  assertLogical(save.mode, len = 1L)
  # Create sub directory, save current wd and set new wd to the new directory
  origin.wd = createDir(sub.dir, save.mode)
  rmd.name = rmdName("BasicReport")

  #define report.id for later accessing in rmd-file writing:
  id = report$report.id
  # TryCatch sets wd back and closes all open connections if an error occurs
  tryCatch({

    report.con = file(rmd.name, "w", encoding = rmdEncoding())
    writeLines("## Basic Summary Report\n", con  = report.con)

    #Load object:
    writeLines(writeRChunkOptions(chunkname = "loadBasicSumObj", id = getId(report)), con = report.con)
    saveLoadObj(report, getId(report), report.con, override = override)
    rmdLibrary("knitr", file = report.con)
    rmdLibrary("kableExtra", file = report.con)
    rmdLibrary("DT", file = report.con)
    writeLines("devtools::load_all() #temporary", con = report.con)
    writeLines("```", con = report.con)

    intro.vec = c(paste("The dataset", writeRinline(paste0(id, "$task$dataset.name")), "is",
      writeRinline(paste0("object.size(", id, "$task$env$data)")), " megabytes in size."), paste0("In total there are ",
        writeRinline(paste0(id, "$task$size")), " observations, ", writeRinline(paste0(id, "$basic.data.summary$basic.summary.list$NAs")), " missing values and ",
        writeRinline(paste0(id, "$basic.data.summary$basic.summary.list$dim")), " columns.\n")
      )
    #Write Basic Text
    writeLines("### Basic Summary\n", con = report.con)
    rmdWriteLines(intro.vec, con = report.con)

    if (!is.null(report$na.summary$na.df)) {
      writeLines("### Missing Value Summary\n", con = report.con)
    }
    writeLines(writeRChunkOptions(chunkname = "NA_summary", id = getId(report)), con = report.con)
    writeLines(paste0("if (!is.null(", getId(report), "$na.summary$na.df)) {"), con = report.con)
    writeLines(paste0("#kable(", report$report.id, "$na.summary$na.df, caption = 'Missing Value Summary', format = 'html') %>% kable_styling(full_width = TRUE)"), con = report.con)
    writeLines(paste0("datatable(",  report$report.id, "$na.summary$na.df, rownames = FALSE, filter = 'bottom', options = list(
  pageLength = 5, autoWidth = TRUE))"), con = report.con)
    writeLines("} \n", con = report.con)
    catf("if (!is.null(%s$na.summary$image)) {", report$report.id, file = report.con)
    writeLines(paste0("print(", report$report.id, "$na.summary$ggplot)"), con = report.con)
    writeLines(paste0("  ", report$report.id, "$na.summary$image()"), con = report.con)
    writeLines("}", con = report.con)
    writeLines("```", con = report.con)

  }, finally = {
    setwd(origin.wd)
    close(report.con)
  })
  return(file.path(sub.dir, rmd.name))
}
