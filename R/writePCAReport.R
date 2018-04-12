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
#'   pca.task = makePCATask(id = "Probe", data = iris, target = "Species",
#'                         tol = 1e-1, center = TRUE)
#'   pca.result = makePCA(pca.task)
#'   pca.report = makePCAReport(pca.result)
#'   writeReport(pca.report, save.mode = FALSE, override = TRUE)
#' @return rmd-file location
#' @import checkmate
#' @export
writeReport.PCAReport = function(report, sub.dir = "Data_Report", save.mode = TRUE, override = FALSE){
  report.env = new.env(parent = .GlobalEnv)
  assertClass(report, "PCAReport")
  assertCharacter(sub.dir, len = 1L, min.chars = 1L)
  assertLogical(save.mode, len = 1L)
  # Create sub directory, save current wd and set new wd to the new directory
  origin.wd = createDir(sub.dir, save.mode)
  rmd.name = rmdName("PCAReport")
  # TryCatch sets wd back and closes all open connections if an error occurs
  tryCatch({
    # Collect all needed packages
    needed.pkgs = getPkgs(report)

    #start the report file
    report.con = file(rmd.name, "w", encoding = rmdEncoding())
    writeLines("## Principal Components Analysis Report for numeric data\n", con  = report.con)
    ##add generic text from vignette
    writeLines("Principial Component Analysis (PCA) is a dimensionality reduction method that uses an an orthogonal transformation
to reduce a large set of (numeric) variables to a small set of (numeric) variables, called principal components, that still contain
most of the information of the large set. Those computed principal components are linearely independent, and hence uncorrelated.
The first principal component accounts for as much of the variability/variance in the data as possible, and each succeeding component
accounts for as much of the remaining variability/variance as possible.\n", con = report.con)

    writeLines(writeRChunkOptions(chunkname = "loadPCAObj", id = getId(report)), con = report.con)
    # load pkgs
    rmdLibrary(needed.pkgs, report.con)
    rmdLibrary("DT", file = report.con)
    #writeLines("devtools::load_all() #temporary", con = report.con)
    # save object and write code to load it in the rmd-file
    saveLoadObj(report, getId(report), report.con, override = override)
    writeLines("```\n", con = report.con)

    writeLines("### Prinicipal Component Analysis Summary Results\n", con = report.con)
    writeLines("In the following the principal component rotation (loadings) and the corresponding scree as well as
      scatterplot will be displayed. \n", con = report.con)

    writeLines("#### Rotation Loadings and Standard Deviation\n", con = report.con)
    writeLines("The following datatable shows the matrix of variable loadings:", con = report.con)
    writeLines(writeRChunkOptions(chunkname = "showRotationAndSD", id = getId(report)), con = report.con)
    writeLines(paste0("dt = DT::datatable(", report$report.id,
      "$pca.result$pca.result$rotation, class = 'compact', filter = 'bottom', options = list(pageLength = 5),
      caption = htmltools::tags$caption(
      style = 'caption-side: bottom; text-align: center;',
      'Table : ', 'Principal Component Rotation Loadings'
      ))"), con = report.con)
    writeLines("dt", con = report.con)
    writeLines("```\n", con = report.con)

    if (length(report$pca.result$pca.result$sdev) > 6) {
      writeLines(paste0("The standard deviations of the 1. to 6. principal component are: ",
        "`r head(", getId(report), "$pca.result$pca.result$sdev) ` ."), con = report.con)
    } else {
      writeLines(paste0("The variances of the 1. to ", length(report$pca.result$pca.result$sdev), ". ", "principial component are: ",
        "`r head(", getId(report), "$pca.result$pca.result$sdev) ` .\n"), con = report.con)
    }

    writeLines("#### Screeplot for principal component analysis\n", con = report.con)
    writeLines("The following plot shows the percentage amount of variance each principal component explaines
      in descending order:\n", con = report.con)
    writeLines(writeRChunkOptions(chunkname = "showScreePlot", id = getId(report),
      options = list(echo = FALSE, message = FALSE, warning = FALSE, out.width = "'90%'")),
      con = report.con)
    writeLines(paste0(report$report.id, "$pca.result$plotlist$pca.scree"), con = report.con)
    writeLines("```\n", con = report.con)

    writeLines("#### Scatterplot for individual observations\n", con = report.con)
    writeLines("The following plot shows how the observations are transformed onto a 2-dimensional space
      using the rotation/loadings for the first two principal components applied to each origin numeric variable:\n",
      con = report.con)
    writeLines(writeRChunkOptions(chunkname = "showScatterPlot", id = getId(report),
      options = list(echo = FALSE, message = FALSE, warning = FALSE, out.width = "'90%'")),
      con = report.con)
    writeLines(paste0(report$report.id, "$pca.result$plotlist$pca.scatter.1"), con = report.con)
    writeLines("```", con = report.con)

  }, finally = {
    setwd(origin.wd)
    close(report.con)
  })
  return(file.path(sub.dir, rmd.name))
}

