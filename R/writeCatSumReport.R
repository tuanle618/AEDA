#' Writes a rmd file for the Categorical Summary Report [WIP]
#'
#' @param cat.sum.report [\code{CatSumReport} Object]\cr
#'   The report Object which should be converted to an rmd file
#' @param sub.dir [\code{character(1)}]\cr
#'   the name of the (relative) sub-directory where the data report will be saved. Default is \code{Data_Report}
#' @param save.mode [\code{logical(1)}]\cr
#'   In Save mode its not possible to use an existing folder.
#'   To ensure no data is lost, a new folder will be created (if possible).
#'   Default is \code{TRUE}
#' @examples
#'  data("Arthritis", package = "vcd")
#'  cat.sum.task = makeCatSumTask(id = "Arthritis.Task", data = Arthritis, target = "Improved")
#'  cat.sum = makeCatSum(cat.sum.task)
#'  #create the numeric summary report
#'  cat.sum.report = makeCatSumReport(cat.sum)
#'  writeReport(cat.sum.report)
#' @return rmd-file location
#' @import checkmate
#' @export
writeReport.CatSumReport = function(cat.sum.report, sub.dir = "Data_Report", save.mode = TRUE){
  report.env = new.env(parent = .GlobalEnv)
  assertClass(cat.sum.report, "CatSumReport")
  assertCharacter(sub.dir, len = 1L, min.chars = 1L)
  assertLogical(save.mode, len = 1L)
  # Create sub directory, save current wd and set new wd to the new directory
  origin.wd = createDir(sub.dir, save.mode)
  rmd.name = rmdName("CatSumReport")

  # TryCatch sets wd back and closes all open connections if an error occurs
  tryCatch({
    ##try part:
    #start the report file
    #  report.con = file(paste0("basicReport_", basic.report$report.task$dataset.name, ".rmd"), "w") #or include task.id ?
    report.con = file(rmd.name, "w")

    writeLines("## Categorical Summary Report from AEDA containing contingency summary as well as plots", con  = report.con)
    writeLines("```{r}", con = report.con)
    writeLines("devtools::load_all()", con = report.con)
    writeLines("#library(AEDA)", con = report.con)

    # save object and write code to load it in the rmd-file
    saveLoadObj(cat.sum.report, getId(cat.sum.report), report.con)
    writeLines("```", con = report.con)

    writeLines("```{r}", con = report.con)
    #testing:
    #vec = c("5+5", "a = TRUE", "print('Hallo')")
    #rmdWriteLines(vec = vec,  con = report.con)
    writeLines("# Declaring object for more convenience and clarity:", con = report.con)
    writeLines(paste0("cat.sum.report.obj = ", cat.sum.report$report.id), con = report.con)
    writeLines("```", con = report.con)

    writeLines("Some text; Categorical Summary ....", con = report.con)
    writeLines("```{r}", con = report.con)
    writeLines(paste0(cat.sum.report$report.id, "$cat.sum$freq"), con = report.con)
    writeLines(paste0(cat.sum.report$report.id, "$cat.sum$contg.list"), con = report.con)
    writeLines(paste0(cat.sum.report$report.id, "$cat.sum$contg.list"), con = report.con)
    writeLines("```", con = report.con)

  }, finally = {
    setwd(origin.wd)
    close(report.con)
  })
  return(file.path(sub.dir, rmd.name))
}
