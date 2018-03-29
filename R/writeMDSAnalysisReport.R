#' Writes a rmd file for the Multidimensional Scaling Report [WIP]
#'
#' @param report [\code{MDSAnalysisReport} Object]\cr
#'   The report Object which should be converted to an rmd file
#' @param sub.dir [\code{character(1)}]\cr
#'   the name of the (relative) sub-directory where the data report will be saved.\cr
#'   Default is \code{Data_Report}
#' @param save.mode [\code{logical(1)}]\cr
#'   In Save mode its not possible to use an existing folder.
#'   To ensure no data is lost, a new folder will be created (if possible).
#' @param override [\code{logical(1)}]\cr
#'   override controls if the function is allowed to override
#'   an existing rmd-file
#'   Default is \code{TRUE}
#' @examples
#'  my.mds.task = makeMDSTask(id = "swiss", data = swiss,
#'   target = "Infant.Mortality")
#'  mds.analysis = makeMDSAnalysis(my.mds.task)
#'  report = makeReport(mds.analysis)
#'  #write the report
#'  writeReport(report, save.mode = FALSE, override = TRUE)
#' @return Invisible NULL
#' @import checkmate
#' @export
writeReport.MDSAnalysisReport = function(report, sub.dir = "Data_Report",
  save.mode = TRUE, override = FALSE){
  report.env = new.env(parent = .GlobalEnv)
  assertClass(report, "MDSAnalysisReport")
  assertCharacter(sub.dir, len = 1L, min.chars = 1L)
  assertLogical(save.mode, len = 1L)
  # Create sub directory, save current wd and set new wd to the new directory
  origin.wd = createDir(sub.dir, save.mode)
  rmd.name = rmdName("MDSAnalysisReport")

  # TryCatch sets wd back and closes all open connections if an error occurs
  tryCatch({
    ##try part:
    #start the report file
    report.con = file(rmd.name, "w", encoding = rmdEncoding())

    writeLines("## Multidimensional Scaling Summary Report", con  = report.con)
    #Load object and libraries:
    writeLines(writeRChunkOptions(chunkname = "loadMDSObj", id = getId(report)),
      con = report.con)
    rmdLibrary("DT", file = report.con)
    writeLines("devtools::load_all() #temporary", con = report.con)
    # save object and write code to load it in the rmd-file
    saveLoadObj(report, getId(report), report.con, override = override)
    writeLines("```", con = report.con)

    ##Add generic text:
    writeLines("### Multidimensional Scaling Summary Results\n", con = report.con)
    writeLines("The following data frame shows the created dimensions for numeric columns from the dataset:",
      con = report.con)
    #Show result dataframe
    writeLines(writeRChunkOptions(chunkname = "showMDSdf", id = getId(report)), con = report.con)

    #plot filterable datatable as option:
    writeLines(paste0("dt = DT::datatable(", report$report.id,
      "$mds.analysis$mds.result.data, class = 'compact', filter = 'bottom', options = list(pageLength = 10),
      caption = htmltools::tags$caption(
      style = 'caption-side: bottom; text-align: center;',
      'Table : ', 'Numeric Summary'
      ))"), con = report.con)
    writeLines("dt", con = report.con)
    writeLines("```\n", con = report.con)

    ##Intro text:
    writeLines("### Multidimensional Scaling Plot\n", con = report.con)
    writeLines("In the following the result of MDS will be shown:", con = report.con)
    writeLines(writeRChunkOptions(chunkname = "showMDSPlot", id = getId(report),
      options = list(echo = FALSE, message = FALSE, warning = FALSE, out.width = "'90%'")),
      con = report.con)
    writeLines(paste0(report$report.id, "$mds.analysis$mds.plot"), con = report.con)
    writeLines("```", con = report.con)

  }, finally = {
    setwd(origin.wd)
    close(report.con)
  })
  return(file.path(sub.dir, rmd.name))
}
