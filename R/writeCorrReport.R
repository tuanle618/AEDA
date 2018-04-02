#' Writes a rmd file for the report [WIP]
#'
#' @param report [\code{*Report} Object]\cr
#'   The report Object which should be converted to an rmd file
#' @param sub.dir [\code{character(1)}]\cr
#'   the name of the directory where the data report will be saved
#' @param save.mode [\code{logical(1)}]\cr
#'   In Save mode its not possible to use an existing folder.
#'   To ensure no data is lost, a new folder will be created (if possible).
#' @param override [\code{logical(1)}]\cr
#'   override controls if the function is allowed to override
#'   an existing rmd-file
#' @examples
#'   my.task = makeCorrTask(id = "test", data = cars)
#'   my.corr = makeCorr(my.task)
#'   report1 = makeCorrReport(my.corr)
#'   writeReport(report1, save.mode = FALSE, override = TRUE)
#' @return rmd-file location
#' @import checkmate
#' @export
writeReport.CorrReport = function(report, sub.dir = "Data_Report", save.mode = TRUE, override = FALSE){
  report.env = new.env(parent = .GlobalEnv)
  # Create sub directory, save current wd and set new wd to the new directory
  origin.wd = createDir(sub.dir, save.mode)
  rmd.name = rmdName("CorrReport")
  # TryCatch sets wd back and closes all open connections if an error occurs
  tryCatch({
      # Collect all needed packages
      needed.pkgs = getPkgs(report)

      #start the report file
      report.con = file(rmd.name, "w", encoding = rmdEncoding())
      writeLines("## Correlation Summary Report\n", con = report.con)
      #writeLines("```{r, echo=FALSE, warning=FALSE, message = FALSE}", con = report.con)
      writeLines(writeRChunkOptions(chunkname = "loadCorrObj", id = getId(report)), con = report.con)

      # load pkgs
      rmdLibrary(needed.pkgs, report.con)
      writeLines("devtools::load_all() #temporary", con = report.con)

      # save object and write code to load it in the rmd-file
      saveLoadObj(report, report$report.id, report.con, override = override)

      writeLines("```", con = report.con)

      writeLines("### Correlation Plot", con = report.con)
      writeLines(paste0("This Plot shows the `r ", idWrapper(report, "method"), "` correlation for interval scaled variables. The size and color of the circles indicate the strength of the correlation."), con = report.con)
      writeLines("```{r, echo=FALSE}", con = report.con)
      rmdWriteLines(report$plot.code$code, con = report.con)
      writeLines("```", con = report.con)

    }, finally = {
      setwd(origin.wd)
      close(report.con)
    })
  return(file.path(sub.dir, rmd.name))
}

