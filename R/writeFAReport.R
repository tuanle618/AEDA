#' Writes a rmd file for the Factor Analysis Report [WIP]
#'
#' @param report [\code{FAReport} Object]\cr
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
#'  library(psych)
#'  data(bfi)
#'  #take small sample of size 200L:
#'  bfi_small = bfi[sample(seq_len(nrow(bfi)), size = 200L), ]
#'  FA.task = makeFATask(id = "bfi", data = bfi_small)
#'  FA.result = makeFA(FA.task)
#'  #create the report
#'  FA.report = makeReport(FA.result)
#'  #write the report
#'  writeReport(FA.report, save.mode = FALSE, override = TRUE)
#' @return Invisible NULL
#' @import checkmate
#' @export
writeReport.FAReport = function(report, sub.dir = "Data_Report",
  save.mode = TRUE, override = FALSE){

  report.env = new.env(parent = .GlobalEnv)
  assertClass(report, "FAReport")
  assertCharacter(sub.dir, len = 1L, min.chars = 1L)
  assertLogical(save.mode, len = 1L)
  # Create sub directory, save current wd and set new wd to the new directory
  origin.wd = createDir(sub.dir, save.mode)
  rmd.name = rmdName("FactorAnalysisReport")

  # TryCatch sets wd back and closes all open connections if an error occurs
  tryCatch({
    ##try part:
    #start the report file
    report.con = file(rmd.name, "w", encoding = rmdEncoding())

    writeLines("## Factor Analysis Summary Report",
      con  = report.con)
    ## add generic text from vignette
    writeLines("Factor analysis is a way to describe variability among observed, correlated variables
in terms of a potentially lower number of unobserved variables, called **factors**. <br>
Like principal component as well as multidimensioanl scaling its purpose is dimension reduction.
It is a way to find hidden patterns, show how those patterns overlap and show what characteristics are
seen in multiple patterns.
These **factors** each embody a set of **observed variables** that have similar response patterns.", con = report.con)
    #Load object and libraries:
    writeLines(writeRChunkOptions(chunkname = "loadFAObj",
      id = getId(report)),
      con = report.con)
    rmdLibrary("DT", file = report.con)
    rmdLibrary("psych", file = report.con)
    #writeLines("devtools::load_all() #temporary", con = report.con)
    # save object and write code to load it in the rmd-file
    saveLoadObj(report, getId(report), report.con, override = override)
    writeLines("```", con = report.con)

    writeLines("### Factor Analysis Summary Results\n", con = report.con)
    ##Analysis text for optimal number of factors:
    if (length(report$task$num.fact.analysis) > 0) {
      writeLines("#### Pre-Analysis for optimal number of factors", con = report.con)
      writeLines("In the following a pre analysis for the optimal number of factors will be displayed:\n",
        con = report.con)
      writeLines(writeRChunkOptions(chunkname = "showParallelAnalysis", id = getId(report),
        options = list(echo = FALSE, message = FALSE, warning = FALSE, out.width = "'90%'")),
        con = report.con)
      writeLines(paste0("plot(", report$report.id, "$task$num.fact.analysis)"), con = report.con)
      writeLines("```\n", con = report.con)
      writeLines(paste0("According to the parallel analysis", " `r ", report$report.id,
        "$task$num.fact.analysis$nfact ` factors should be taken.\n"),
        con = report.con)
    }

    ##Add generic text:
    writeLines("#### Factor Loadings",
      con = report.con)
    writeLines("The following data frame shows the factor loadings for each numeric column.
      Note that the rows are the numeric columns.",
      con = report.con)
    #Show result dataframe
    writeLines(writeRChunkOptions(chunkname = "showFAloadings", id = getId(report)), con = report.con)

    #plot filterable datatable as option:
    writeLines(paste0("loadingsWithCom = round(cbind.data.frame(as.data.frame(unclass(", getId(report),
      "$fa.result$loadings)), communalities = ", getId(report), "$fa.result$communalities), 4)"), con = report.con)
    writeLines("dt = DT::datatable(loadingsWithCom, class = 'compact', filter = 'bottom',
      options = list(pageLength = 10), caption = htmltools::tags$caption(
      style = 'caption-side: bottom; text-align: center;',
      'Table : ', 'Factor Loadings'))", con = report.con)
    writeLines("dt", con = report.con)
    writeLines("```\n", con = report.con)

    #Plot factor graph
    writeLines("#### Factor Analysis Graph", con = report.con)
    writeLines("The following graphs visualizes the factor loadings. A cutoff of 0.3 will be applied:\n",
      con = report.con)

    writeLines(writeRChunkOptions(chunkname = "showFAgraph", id = getId(report)), con = report.con)
    writeLines(paste0("fa.diagram(", report$report.id, "$fa.result, cut = 0.3)"), con = report.con)
    writeLines("```\n", con = report.con)

  }, finally = {
    setwd(origin.wd)
    close(report.con)
  })
  return(file.path(sub.dir, rmd.name))
}

