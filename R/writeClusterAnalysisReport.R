#' Writes a rmd file for the ClusterAnalysis Report [WIP]
#'
#' @param report [\code{BasicReport} Object]\cr
#'   The report Object which should be converted to an rmd file
#' @param sub.dir [\code{character(1)}]\cr
#'   the name of the (relative) sub-directory where the data report will be saved.\cr
#'   Default is \code{Data_Report}
#' @param save.mode [\code{logical(1)}]\cr
#'   In Save mode its not possible to use an existing folder.\cr
#'   To ensure no data is lost, a new folder will be created (if possible).
#'   Default is \code{TRUE}
#' @param override [\code{logical(1)}]\cr
#'   override controls if the function is allowed to override\cr
#'   an existing rmd-file
#' @examples
#' \dontrun{
#'   cluster.task = makeClusterTask(id = "iris", data = iris,
#'    method = "cluster.kmeans")
#'   cluster.analysis.result = makeClusterAnalysis(cluster.task)
#'   cluster.analysis.report = makeClusterAnalysisReport(cluster.analysis)
#'   writeReport(report, save.mode = FALSE, override = TRUE)
#' }
#' @return Invisible NULL
#' @import checkmate
#' @export
writeReport.ClusterAnalysisReport = function(report, sub.dir = "Data_Report", save.mode = TRUE, override = FALSE){
  report.env = new.env(parent = .GlobalEnv)
  assertClass(report, "ClusterAnalysisReport")
  assertCharacter(sub.dir, len = 1L, min.chars = 1L)
  assertLogical(save.mode, len = 1L)
  # Create sub directory, save current wd and set new wd to the new directory
  origin.wd = createDir(sub.dir, save.mode)
  rmd.name = rmdName("ClusterAnalysisReport")

  #define report.id for later accessing in rmd-file writing:
  id = report$report.id
  # TryCatch sets wd back and closes all open connections if an error occurs
  tryCatch({
    ##try part
    report.con = file(rmd.name, "w", encoding = rmdEncoding())

    #Load object
    writeLines("## Cluster Summary Report \n", con  = report.con)
    #insert generic text from vignette:
    writeLines("Cluster analysis is a unsupervised learning task, which mainly focuses on
grouping a set of objects in such a way that objects in the same group (called a cluster)
are more similar (in some sense) to each other than to those in other groups (clusters).
Cluster analysis itself is not one specific algorithm, but the general task to be solved.
It can be achieved by various algorithms that differ significantly in their notion of what
constitutes a cluster and how to efficiently find them.", con = report.con)

    #writeLines("```{r loadClusterObj_XYZid, echo=FALSE, warning=FALSE, message = FALSE}", con = report.con)
    writeLines(writeRChunkOptions(chunkname = "loadClusterObj", id = getId(report)), con = report.con)
    # save object and write code to load it in the rmd-file
    saveLoadObj(report, getId(report), report.con, override = override)
    # load/require libraries
    rmdLibrary("knitr", file = report.con)
    rmdLibrary("kableExtra", file = report.con)
    #temporary:
    #writeLines("devtools::load_all() #temporary", con = report.con)
    writeLines("```\n", con = report.con)

    ### Overview: All numeric columns
    writeLines("### Overview: All numeric columns \n", con  = report.con)
    txt = paste0("The dataset contains of `r ", "length(", getId(report),
      "$report.task$numdatatypes$numeric) + length(", getId(report),
      "$report.task$numdatatypes$integer)`")
    txt = paste(txt, "numeric columns.")
    writeLines(txt, con = report.con)
    #PCA text only for not hierarchical methods:
    if (!is.element(report$report.task$method, c("cluster.h", "cluster.agnes", "cluster.diana"))) {
      if (length(report$report.task$numdatatypes$numeric) + length(report$report.task$numdatatypes$integer) > 2) {
        writeLines("Since the number of numeric columns is greater than 2, for **vizualization**
        we compute a principal component analysis and apply the cluster analysis to the respective two principal components: ",
          con = report.con)
      }
    }
    #Diagnostics for everything except 'cluster.dbscan' and 'cluster.kkmeans
    if (!is.element(report$report.task$method, c("cluster.kkmeans", "cluster.dbscan"))) {
      writeLines("\n", con = report.con)
      writeLines("#### Diagnostics \n", con = report.con)
      writeLines("The following diagnostic plots show how the optimal number of cluster is selected:", con = report.con)
      #writeLines("```{r plotClusterDiag_XYZid, echo=FALSE, warning=FALSE, message = FALSE}", con = report.con)
      #writeLines(writeRChunkOptions(chunkname = "plotClusterDiag", id = getId(report),
      #  options = list(echo = FALSE, warning = FALSE, message = FALSE, fig.height = 4)), #fig.dim = c(6,6)
      #  con = report.con)
      writeLines(writeRChunkOptions(chunkname = "plotClusterDiag", id = getId(report)), con = report.con)
      txt = paste0("multiplot(plotlist = ", getId(report), "$cluster.analysis$cluster.all$cluster.diag, cols = 2)")
      writeLines(txt, con = report.con)
      writeLines("```\n", con = report.con)
    }

    #Cluster Plot Result
    writeLines("#### Cluster Plot Result \n", con = report.con)
    writeLines("Applying cluster algorithmus we receive following cluster plot:", con = report.con)
    #writeLines("```{r plotCluster_XYZid, echo=FALSE, warning=FALSE, message = FALSE}", con = report.con)
    #writeLines(writeRChunkOptions(chunkname = "plotCluster", id = getId(report),
    #  options = list(echo = FALSE, warning = FALSE, message = FALSE, fig.height = 4)),
    #  con = report.con)
    writeLines(writeRChunkOptions(chunkname = "plotCluster", id = getId(report)), con = report.con)
    writeLines(paste0("print(", getId(report), "$cluster.analysis$cluster.all$cluster.plot)"), con = report.con)
    writeLines("```\n", con = report.con)

    #Cluster Result only for kmeans and pam algorithm
    if (is.element(report$report.task$method, c("cluster.kmeans", "cluster.pam"))) {
      writeLines("#### Cluster Result \n", con = report.con)
      if (length(report$report.task$numdatatypes$numeric) + length(report$report.task$numdatatypes$integer) > 2) {
        txt = "Since Prinicipal components was only for **vizualization** but the clustering algorithm still can
handle multidimensional data we receive after transforming the centers from the principal components clusters:"
        writeLines(txt, con = report.con)
      } else {
        writeLines("Applying cluster algorithmus we receive following cluster result:", con = report.con)
      }
      #write R-Chunk:
      #writeLines("```{r ClusterRes_XYZid, echo=FALSE, warning=FALSE, message = FALSE}", con = report.con)
      writeLines(writeRChunkOptions(chunkname = "ClusterRes", id = getId(report)), con = report.con)
      if (report$report.task$method == "cluster.kmeans") {
        caption = "Clustering Centers"
        txt = paste0("kable(", getId(report), "$cluster.analysis$cluster.all$cluster.res$centers,
        format = 'html', caption = ", "'", caption, "')", "%>% kable_styling(full_width = F, position = 'left')")
      } else {
        caption = "Clustering Medoids"
        txt = paste0("kable(", getId(report), "$cluster.analysis$cluster.all$cluster.res$medoids,
        format = 'html', caption = ", "'", caption, "')", "%>% kable_styling(full_width = F, position = 'left')")
      }
      writeLines(txt, con = report.con)
      writeLines("```\n", con = report.con)
    }
    #Combinations only for not-hierarchical methods:
    if (!is.element(report$report.task$method, c("cluster.h", "cluster.agnes", "cluster.diana"))) {
      ### Overview: Combinations for numeric columns of dataset
      writeLines("### Overview: Combinations for numeric columns of dataset \n", con = report.con)
      #writeLines("```{r clusterCombPlots_XYZid, echo=FALSE, warning=FALSE, message = FALSE}", con = report.con)
      #writeLines(writeRChunkOptions(chunkname = "clusterCombPlots", id = getId(report),
      #  options = list(echo = FALSE, warning = FALSE, message = FALSE, fig.height = 4)),
      #  con = report.con)
      writeLines(writeRChunkOptions(chunkname = "clusterCombPlots", id = getId(report),
        options = list(echo = FALSE, message = FALSE, warning = FALSE,  fig.dim = c(9, 5), out.width = "'50%'")),
        con = report.con)
      #txt = paste0("multiplot(plotlist = lapply(", getId(report) ,"$cluster.analysis$comb.cluster.list, FUN = `[[`, 'cluster.plot'), cols = 2)")
      txt = paste0("multiplotPages(plotlist = lapply(", getId(report),
        "$cluster.analysis$comb.cluster.list, FUN = `[[`, 'cluster.plot'), k = 2, no.cols = 2)")
      writeLines(txt, con = report.con)
      writeLines("```", con = report.con)
    }
  }, finally = {
    setwd(origin.wd)
    close(report.con)
  })
  return(file.path(sub.dir, rmd.name))
}

