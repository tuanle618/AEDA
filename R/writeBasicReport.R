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
#'   my.report.task = makeReportTask(id = "test.report", data = airquality, target = "Wind")
#'   report = makeBasicReport(my.report.task, data = airquality)
#'   writeReport(report, save.mode = FALSE, override = TRUE)
#'
#'   data("iris")
#'   my.report.task2 = makeReportTask(id = "test.report2", data = iris, target = "Species")
#'   report2 = makeBasicReport(my.report.task2, data = iris)
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
    ##try part:
    #start the report file
  #  report.con = file(paste0("basicReport_", report$report.task$dataset.name, ".rmd"), "w") #or include task.id ?
    report.con = file(rmd.name, "w", encoding = rmdEncoding())
    writeLines("## Basic data and missing values summary", con  = report.con)
    writeLines("```{r, echo=FALSE, warning=FALSE, message = FALSE}", con = report.con)

    #  #save data
  #  if (save.mode) {
  #    #file.name = paste0(report$report.task$dataset.name, ".rds")
  #    #saveRDS(report$report.task$env$data, file = file.name)
  #  }
  #  # load data
  #  data.path = file.path(".", report$report.task$dataset.name)
  #  rmdloadData(report$report.task$dataset.name, data.path, report.con)

    # save object and write code to load it in the rmd-file
    saveLoadObj(report, getId(report), report.con, override = override)

    writeLines("```", con = report.con)


    #writeLines("```{r}", con = report.con)
    #testing:
    #vec = c("5+5", "a = TRUE", "print('Hallo')")
    #rmdWriteLines(vec = vec,  con = report.con)

    #writeLines("# Declaring object for more convenience and clarity:", con = report.con)
    #writeLines(paste0("report.obj = ", report$report.id), con = report.con)
    #writeLines("```", con = report.con)

    intro.vec = c(paste("The dataset", writeRinline(paste0(id, "$report.task$dataset.name")), "is",
      writeRinline(paste0("object.size(", id, "$report.task$env$data)")), " megabytes in size."), paste0("In total there are ",
        writeRinline(paste0(id, "$report.task$size")), " observations, ", writeRinline(paste0(id, "$basic.data.summary$basic.summary.list$NAs")), " missing values and ",
        writeRinline(paste0(id, "$basic.data.summary$basic.summary.list$dim")), " columns.")
      )

    writeLines("### Overview", con = report.con)
    rmdWriteLines(intro.vec, con = report.con)

    writeLines("```{r, echo=FALSE, warning=FALSE, message=FALSE}", con = report.con)
    rmdLibrary("knitr", file = report.con)
    rmdLibrary("kableExtra", file = report.con)
    writeLines(paste0("#", report$report.id, "$report.task"), con = report.con)
    #writeLines(paste0(report$report.id, "$na.summary$na.df"), con = report.con)
    writeLines(paste0("kable(", report$report.id, "$na.summary$na.df, caption = 'Missing Value Summary', format = 'html') %>%
  kable_styling(full_width = TRUE)"), con = report.con)

    writeLines("#Plotting missing values according to their frequency", con = report.con)
    writeLines(paste0(report$report.id, "$na.summary$ggplot"), con = report.con)
    writeLines("#Plotting missing values according to their index", con = report.con)
    catf("if (!is.null(%s$na.summary$image)) {", report$report.id, file = report.con)
    writeLines(paste0("  ", report$report.id, "$na.summary$image()"), con = report.con)
    writeLines("}", con = report.con)
    writeLines("```", con = report.con)

  }, finally = {
    setwd(origin.wd)
    close(report.con)
  })
  return(file.path(sub.dir, rmd.name))
}
