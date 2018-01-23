#' Writes a rmd file for the Numeric Summary Report [WIP]
#'
#' @param num.sum.report [\code{NumSumReport} Object]\cr
#'   The report Object which should be converted to an rmd file
#' @param sub.dir [\code{character(1)}]\cr
#'   the name of the (relative) sub-directory where the data report will be saved. Default is \code{Data_Report}
#' @param save.mode [\code{logical(1)}]\cr
#'   In Save mode its not possible to use an existing folder.
#'   To ensure no data is lost, a new folder will be created (if possible).
#'   Default is \code{TRUE}
#' @examples
#' #'  data("Boston", package = "MASS")
#'  num.sum.task = makeNumSumTask(id = "BostonTask", data = Boston, target = "medv")
#'  #get the numeric summary task object
#'  num.sum = makeNumSum(num.sum.task)
#'  #create the numeric summary report
#'  num.sum.report = makeNumSumReport(num.sum)
#'  #write the report
#'  writeNumSumReport(num.sum.report)
#' @return Invisible NULL
#' @import checkmate
#' @export
writeReport.NumSumReport = function(num.sum.report, sub.dir = "Data_Report", save.mode = TRUE){
  assertClass(num.sum.report, "NumSumReport")
  assertCharacter(sub.dir, len = 1L, min.chars = 1L)
  assertLogical(save.mode, len = 1L)
  # Create sub directory, save current wd and set new wd to the new directory
  origin.wd = createDir(sub.dir, save.mode)
  rmd.name = rmdName("NumSumReport")

  # TryCatch sets wd back and closes all open connections if an error occurs
  tryCatch({
    ##try part:
    #start the report file
    #  report.con = file(paste0("basicReport_", basic.report$report.task$dataset.name, ".rmd"), "w") #or include task.id ?
    report.con = file(rmd.name, "w")

    writeLines("## Numeric Summary Report from AEDA containing numeric summary as well as plots", con  = report.con)
    writeLines("```{r}", con = report.con)
    writeLines("devtools::load_all()", con = report.con)
    writeLines("#library(AEDA)", con = report.con)

    # save object and write code to load it in the rmd-file
    saveLoadObj(num.sum.report, deparse(substitute(num.sum.report)), report.con)

    writeLines("```", con = report.con)

    writeLines("```{r}", con = report.con)
    #testing:
    #vec = c("5+5", "a = TRUE", "print('Hallo')")
    #rmdWriteLines(vec = vec,  con = report.con)
    writeLines("# Declaring object for more convenience and clarity:", con = report.con)
    writeLines(paste0("num.sum.report.obj = ", num.sum.report$report.id), con = report.con)
    writeLines("```", con = report.con)

    writeLines("Some text; Numeric Summary ....", con = report.con)
    writeLines("```{r}", con = report.con)
    writeLines(paste0(num.sum.report$report.id, "$num.sum.df"), con = report.con)
    writeLines(paste0(num.sum.report$report.id, "$num.sum.var"), con = report.con)
    writeLines("```", con = report.con)

  }, finally = {
    setwd(origin.wd)
    close(report.con)
  })
  return(invisible(NULL))
}
