#' Writes a rmd file for the ClusterAnalysis Report [WIP]
#'
#' @param cluster.report [\code{BasicReport} Object]\cr
#'   The report Object which should be converted to an rmd file
#' @param sub.dir [\code{character(1)}]\cr
#'   the name of the (relative) sub-directory where the data report will be saved. Default is \code{Data_Report}
#' @param save.mode [\code{logical(1)}]\cr
#'   In Save mode its not possible to use an existing folder.
#'   To ensure no data is lost, a new folder will be created (if possible).
#'   Default is \code{TRUE}
#' @examples
#'   my.cluster.task = makeClusterTask(id = "iris", data = iris, target = "Species", method = "cluster.kmeans")
#'   cluster.analysis = makeClusterAnalysis(my.cluster.task)
#'   cluster.report = makeClusterAnalysisReport(cluster.analysis)
#'   writeReport(cluster.report)
#' @return Invisible NULL
#' @import checkmate
#' @export
writeReport.ClusterAnalysisReport = function(cluster.report, sub.dir = "Data_Report", save.mode = TRUE){
  report.env = new.env(parent = .GlobalEnv)
  assertClass(cluster.report, "ClusterAnalysisReport")
  assertCharacter(sub.dir, len = 1L, min.chars = 1L)
  assertLogical(save.mode, len = 1L)
  # Create sub directory, save current wd and set new wd to the new directory
  origin.wd = createDir(sub.dir, save.mode)
  rmd.name = rmdName("ClusterAnalysisReport")

  #define report.id for later accessing in rmd-file writing:
  id = cluster.report$report.id
  # TryCatch sets wd back and closes all open connections if an error occurs
  tryCatch({
    ##try part
    report.con = file(rmd.name, "w", encoding = rmdEncoding())
    writeLines("## Cluster Analysis Report for numeric data", con  = report.con)
    writeLines("```{r, echo=FALSE, warning=FALSE, message = FALSE}", con = report.con)

    # save object and write code to load it in the rmd-file
    saveLoadObj(cluster.report, getId(cluster.report), report.con)

    writeLines("```", con = report.con)

  }, finally = {
    setwd(origin.wd)
    close(report.con)
  })
  return(file.path(sub.dir, rmd.name))
}
