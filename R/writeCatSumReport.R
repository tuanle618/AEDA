#' Writes a rmd file for the Categorical Summary Report [WIP]
#'
#' @param report [\code{CatSumReport} Object]\cr
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
#'  data("Arthritis", package = "vcd")
#'  cat.sum.task = makeCatSumTask(id = "Arthritis.Task", data = Arthritis, target = "Improved")
#'  cat.sum = makeCatSum(cat.sum.task)
#'  #create the numeric summary report
#'  report = makeCatSumReport(cat.sum)
#'  writeReport(report)
#' @return rmd-file location
#' @import checkmate
#' @export
writeReport.CatSumReport = function(report, sub.dir = "Data_Report", save.mode = TRUE, override = FALSE){
  report.env = new.env(parent = .GlobalEnv)
  assertClass(report, "CatSumReport")
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
    report.con = file(rmd.name, "w", encoding = rmdEncoding())

    writeLines("## Categorical Summary Report from AEDA containing contingency summary as well as plots", con  = report.con)
    writeLines("```{r, echo=FALSE, warning=FALSE, message = FALSE}", con = report.con)

    # save object and write code to load it in the rmd-file
    saveLoadObj(report, getId(report), report.con, override = override)
    writeLines("```", con = report.con)

    writeLines("```{r}", con = report.con)
    #testing:
    #vec = c("5+5", "a = TRUE", "print('Hallo')")
    #rmdWriteLines(vec = vec,  con = report.con)
    writeLines("# Declaring object for more convenience and clarity:", con = report.con)
    writeLines(paste0("report.obj = ", report$report.id), con = report.con)
    writeLines("```", con = report.con)

    writeLines("Some text; Categorical Summary ....", con = report.con)
    writeLines("```{r, echo=FALSE}", con = report.con)
    writeLines(paste0(report$report.id, "$cat.sum$freq"), con = report.con)
    writeLines(paste0(report$report.id, "$cat.sum$contg.list"), con = report.con)
    writeLines(paste0("multiplot(", report$report.id, "$cat.sum$plot.list", ", cols = 2)"),
      con = report.con)
    writeLines("```", con = report.con)

  }, finally = {
    setwd(origin.wd)
    close(report.con)
  })
  return(file.path(sub.dir, rmd.name))
}
