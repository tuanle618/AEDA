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
#'   test.task = makePCATask(id = "Probe", data = iris, target = "Petal.Length",
#'                         tol = 1e-1, center = TRUE)
#'   my.pca2 = makePCA(test.task)
#'   report2 = makePCAReport(my.pca2)
#'   writeReport(report2, save.mode = FALSE, override = TRUE)
#' @return rmd-file location
#' @import checkmate
#' @export
writeReport.PCAReport = function(report, sub.dir = "Data_Report", save.mode = TRUE, override = FALSE){
  report.env = new.env(parent = .GlobalEnv)
  # Create sub directory, save current wd and set new wd to the new directory
  origin.wd = createDir(sub.dir, save.mode)
  rmd.name = rmdName("PCAReport")
  # TryCatch sets wd back and closes all open connections if an error occurs
  tryCatch({
    # Collect all needed packages
    needed.pkgs = getPkgs(report)

    #start the report file
    report.con = file(rmd.name, "w", encoding = rmdEncoding())
    writeLines("## Principal Components Analysis Report for numeric data", con  = report.con)
    writeLines("```{r, echo=FALSE, warning=FALSE, message = FALSE}", con = report.con)
    #writeLines("```{r, echo=FALSE, warning=FALSE, message = FALSE}", con = report.con)
    writeLines(writeRChunkOptions(chunkname = "loadPCAObj", id = getId(report)), con = report.con)

    # load pkgs
    rmdLibrary(needed.pkgs, report.con)

    # save object and write code to load it in the rmd-file
    saveLoadObj(report, report$report.id, report.con, override = override)

    writeLines("```", con = report.con)

    writeLines("## PCA Plot", con = report.con)
    writeLines("```{r, echo=FALSE}", con = report.con)
    writeLines("```", con = report.con)

  }, finally = {
    setwd(origin.wd)
    close(report.con)
  })
  return(file.path(sub.dir, rmd.name))
}

