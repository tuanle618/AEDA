#' Writes a rmd file for the Numeric Summary Report [WIP]
#'
#' @param num.sum.report [\code{NumSumReport} Object]\cr
#'   The report Object which should be converted to an rmd file
#' @param sub.dir [\code{character(1)}]\cr
#'   the name of the (relative) sub-directory where the data report will be saved. Default is \code{Data_Report}
#' @param save.mode [\code{logical(1)}]\cr
#'   In Save mode its not possible to use an existing folder.
#'   To ensure no data is lost, a new folder will be created (if possible).
#'   Default is \code{TRUE}
#' @examples
#'  data("Boston", package = "MASS")
#'  num.sum.task = makeNumSumTask(id = "BostonTask", data = Boston, target = "medv")
#'  #get the numeric summary task object
#'  num.sum = makeNumSum(num.sum.task)
#'  #create the numeric summary report
#'  num.sum.report = makeNumSumReport(num.sum)
#'  #write the report
#'  writeReport(num.sum.report)
#' @return Invisible NULL
#' @import checkmate
#' @export
writeReport.NumSumReport = function(num.sum.report, sub.dir = "Data_Report", save.mode = TRUE){
  report.env = new.env(parent = .GlobalEnv)
  assertClass(num.sum.report, "NumSumReport")
  assertCharacter(sub.dir, len = 1L, min.chars = 1L)
  assertLogical(save.mode, len = 1L)
  # Create sub directory, save current wd and set new wd to the new directory
  origin.wd = createDir(sub.dir, save.mode)
  rmd.name = rmdName("NumSumReport")

  # TryCatch sets wd back and closes all open connections if an error occurs
  tryCatch({
    ##try part:
    #start the report file
    #  report.con = file(paste0("basicReport_", basic.report$report.task$dataset.name, ".rmd"), "w") #or include task.id ?
    report.con = file(rmd.name, "w", encoding = rmdEncoding())

    writeLines("## Numeric Summary Report from AEDA containing numeric summary as well as plots", con  = report.con)
    writeLines("```{r, echo=FALSE, warning=FALSE, message = FALSE}", con = report.con)
    rmdLibrary("knitr", file = report.con)
    rmdLibrary("kableExtra", file = report.con)
    rmdLibrary("DT", file = report.con)
    # save object and write code to load it in the rmd-file
    saveLoadObj(num.sum.report, getId(num.sum.report), report.con)
    writeLines("```", con = report.con)

    writeLines("```{r, echo=FALSE}", con = report.con)
    #testing:
    #vec = c("5+5", "a = TRUE", "print('Hallo')")
    #rmdWriteLines(vec = vec,  con = report.con)
    writeLines("# Declaring object for more convenience and clarity:", con = report.con)
    writeLines(paste0("num.sum.report.obj = ", num.sum.report$report.id), con = report.con)
    writeLines("```", con = report.con)

    writeLines("Some text; Numeric Summary ....", con = report.con)
    writeLines("```{r, echo=FALSE}", con = report.con)
    #writeLines(paste0("kable(",num.sum.report$report.id, "$num.sum.df[,c(5,6,4,7,8,10,12,13,14,16,21,22)])"), con = report.con)
    writeLines(paste0("kable(", num.sum.report$report.id, "$num.sum.df[,c(5,6,4,7,8,10,12,13,14,16,21,22)], caption = 'Numeric Summary', format = 'html') %>%
      kable_styling(full_width = F) %>%
      footnote(general = 'Following footnotes explain some measure from the table above ',
              alphabet = c('Kurtosis will be calculated via: frac{sum_{i = 1}^{n}(x_i - bar{x})^4 / n}{s^4};', 'Skewness will be calculated via: frac{sum_{i = 1}^{n}(x_i - bar{x})^3 / n}{s^3}; ',
      'l.bound is defined as: q_0.25 - 1.5*IQR; ', 'u.bound is defined as: q_0.75 + 1.5*IQR; where IQR is defined as IQR:= q_0.75 - q_0.25'),
      general_title = 'General: ',
      alphabet_title = 'Explanation: ',
      footnote_as_chunk = T
    )"),
      con = report.con)
    writeLines(paste0("dt = DT::datatable(", num.sum.report$report.id, "$num.sum.df[,c(5,6,4,7,8,10,12,13,14,16,21,22)], class = 'compact', filter = 'bottom', options = list(pageLength = 10),
      caption = htmltools::tags$caption(
        style = 'caption-side: bottom; text-align: center;',
        'Table : ', 'Numeric Summary'
      ))"), con = report.con)
    writeLines("dt", con = report.con)
    writeLines(paste0("#", num.sum.report$report.id, "$num.sum.var"), con = report.con)
    writeLines(paste0("invisible(lapply(", num.sum.report$report.id, "$num.sum.var,", " FUN = function(x) {
    multiplot(plotlist = list(plot.hist = x[[3]], plot.box = x[[4]]), cols = 2)
    }))"), con = report.con)
    writeLines("```", con = report.con)

  }, finally = {
    setwd(origin.wd)
    close(report.con)
  })
  return(file.path(sub.dir, rmd.name))
}
