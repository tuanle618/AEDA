#' @title Generates code for a Correlation Plot
#'
#' @description
#' generates code for a Correlation Plot
#' @param corr.object [\code{CorrTask}]\cr
#'   A corrTask Object
#' @param obj.name [\code{character()}]
#'   the name of object from where to get the data.
#'   make(Corr)Report function generates a random string for this.
#' @return R Code as string
#'
#' @import checkmate
generateCorrPlot = function(corr.object, obj.name) {
  assertClass(corr.object, "CorrObj")

  plot.code = paste0("ggcorr(data = NULL, cor_matrix = ", obj.name, "$corr.matrix", ", geom = \"circle\", nbreaks = 10)")
  makeS3Obj("PlotCode",
    needed.pkg = "GGally",
    code = plot.code)
}
# Intern example
# my.task = makeCorrTask(id = "test", data = cars)
# my.corr = makeCorr(my.task)
# generateCorrPlot(my.corr, "test")
