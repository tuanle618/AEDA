#' Writes and rmd file for the report WIP
#' @param x
#' @param y
#'
writeReport = function(x,y){

  needed.pkgs = unique(c(x$task$needed.pkgs, y$needed.pkg))
  report <- file("report.rmd", "w")
  writeLines("```", con = report)
  catf("library(%s)\n",needed.pkgs, file = report)
  catf("setwd(\"%s\")",x$task$data.path, file = report)
  catf("load(file = \"%s\")", x$task$data.name, file = report)
  writeLines("```", con = report)
  writeLines("SOme text; CorrPlot ....", con = report)
  writeLines("```", con = report)
  writeLines(y$code[[1]], con = report)
  writeLines("```", con = report)
  close(zz)
}

save(cars, file="cars")
my.task = makeCorrTask(id = "test", data = cars)
my.task$data
my.corr = makeCorr(my.task)
my.plot = generateCorrPlot(my.corr)
writeReport(my.corr, my.plot)
