#' Writes a rmd file for the report [WIP]
#'
#' @param report [\code{*Report} Object]\cr
#'   The report Object which should be converted to an rmd file
#' @param sub.dir [\code{character(1)}]\cr
#'   the name of the directory where the data report will be saved
#' @param save.mode [\code{logical(1)}]\cr
#'   In Save mode its not possible to use an existing folder.
#'   To ensure no data is lost, a new folder will be created (if possible).
#' @examples
#'   my.task = makeCorrTask(id = "test", data = cars)
#'   my.corr = makeCorr(my.task)
#'   report1 = makeCorrReport(my.corr, type = "CorrPlot")
#'   writeCorrReport(report1)
#' @return rmd-file location
#' @import checkmate
#' @export
writeCorrReport = function(report, sub.dir = "Data_Report", save.mode = TRUE){
  # Create sub directory, save current wd and set new wd to the new directory
  origin.wd = createDir(sub.dir, save.mode)
  rmd.name = rmdName("CorrReport")
  # TryCatch sets wd back and closes all open connections if an error occurs
  tryCatch({
      # Collect all needed packages
      needed.pkgs = getPkgs(report)

      #start the report file
      report.con = file(rmd.name, "w")
      writeLines("```{r}", con = report.con)

      # load pkgs
      rmdLibrary(needed.pkgs, report.con)

      # save object and write code to load it in the rmd-file
      saveLoadObj(report, deparse(substitute(report)), report.con)

      writeLines("```", con = report.con)
      writeLines("Some text; CorrPlot ....", con = report.con)
      writeLines("```{r}", con = report.con)
      rmdWriteLines(report$plot.code$code, con = report.con)
      writeLines("```", con = report.con)

    }, finally = {
      setwd(origin.wd)
      close(report.con)
    })
  return(file.path(sub.dir, rmd.name))
}

### Example
# my.task = makeCorrTask(id = "test", data = cars)
# my.corr = makeCorr(my.task)
# report1 = makeCorrReport(my.corr, type = "CorrPlot")
# writeCorrReport(report1)
#
# data(diamonds, package = "ggplot2")
# my.task = makeCorrTask(id = "test", data = diamonds)
# my.corr = makeCorr(my.task)
# report1 = makeCorrReport(my.corr, type = "CorrPlot")
# writeReport(report1)
