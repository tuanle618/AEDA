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
#'  writeReport(report, save.mode = FALSE, override = TRUE)
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

    report.con = file(rmd.name, "w", encoding = rmdEncoding())

    writeLines("## Categorical Summary Report\n", con  = report.con)

    #Load cat sum object
    writeLines(writeRChunkOptions(chunkname = "loadCatSumObj", id = getId(report)), con = report.con)
    rmdLibrary("knitr", file = report.con)
    rmdLibrary("kableExtra", file = report.con)
    writeLines("devtools::load_all() #temporary", con = report.con)
    # save object and write code to load it in the rmd-file
    saveLoadObj(report, getId(report), report.con, override = override)
    writeLines("```", con = report.con)

    ##show contingency:: use single chunk so windows is not open in browser
    writeLines("### Categorical Summary Results\n", con = report.con)
    writeLines("In the following contingency tables for categorical columns will be displayed:", con = report.con)

    #1-D Contingency
    for (i in seq_len(length(report$cat.sum$freq))) {
      #Define Chunk options:
      writeLines(writeRChunkOptions(chunkname = paste0("contingencyOneD_", i), id = getId(report),
        options = list(echo = FALSE, message = FALSE, warning = FALSE, results = "'asis'")), con = report.con)
      txt = paste0("kable_styling(kable_input = kable(", getId(report), "$cat.sum$freq[[", i, "]], format = 'html',
        caption = '1-D Contingency Table ", i, "')", ", full_width = TRUE)")
      writeLines(txt, con = report.con)
      #Close chunk
      writeLines("```", con = report.con)
    }
    #2-D Contingency
    for (i in seq_len(length(report$cat.sum$contg.list))) {
      #Define Chunk options:
      writeLines(writeRChunkOptions(chunkname = paste0("contingencTwoD_", i), id = getId(report),
        options = list(echo = FALSE, message = FALSE, warning = FALSE, results = "'asis'")), con = report.con)
      txt = paste0("kable_styling(kable_input = kable(", getId(report), "$cat.sum$contg.list[[", i, "]], format = 'html',
        caption = '2-D Contingency Table ", i, "')", ", full_width = TRUE)")
      writeLines(txt, con = report.con)
      #Close chunk
      writeLines("```", con = report.con)
    }

    #Show barplots:
    writeLines("### Categorical Summary Results\n", con = report.con)
    writeLines("In the following bar plots for categorical columns will be displayed:", con = report.con)
    writeLines(writeRChunkOptions(chunkname = "showBarPlots", id = getId(report),
      options = list(echo = FALSE, message = FALSE, warning = FALSE, results = "'asis'", out.width = "'100%'")),
      con = report.con)
    writeLines(paste0("multiplot(plotlist = ", report$report.id, "$cat.sum$plot.list", ", cols = 2)"),
      con = report.con)
    writeLines("```", con = report.con)

  }, finally = {
    setwd(origin.wd)
    close(report.con)
  })
  return(file.path(sub.dir, rmd.name))
}
