#' @title Generates code for a Correlation Plot
#'
#' @description
#' generates code for a Correlation Plot
#' @param corr.object [\code{CorrTask}]\cr
#'   A corrTask Object
#' @return R Code as string
#'
#' @import checkmate
generateCorrPlot = function(corr.object) {
  assertClass(corr.object, "CorrObj")

  arg.name = deparse(substitute(corr.object))
  plot.code = paste0("corrplot(", arg.name, "$corr.matrix)")
  makeS3Obj("PlotCode",
    needed.pkg = "corrplot",
    code = list(plot.code))
}
# Intern example
# my.task = makeCorrTask(id = "test", data = cars)
# my.corr = makeCorr(my.task)
# generateCorrPlot(my.corr)






