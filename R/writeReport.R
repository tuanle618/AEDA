#' Writes and rmd file for the report WIP
#' @param x
#' @param dir
#'   directory where the sub directory is created
#'
writeReport = function(x, sub.dir = "Data_Report"){
  # Create sub directory, save current wd and set new wd to the new directory
  origin.wd = createDir(sub.dir)

  # Collect all needed packages
  needed.pkgs = unique(c(x$corr.obj$task$needed.pkgs, x$plot.code$needed.pkg))

  #start the report file
  report <- file("report.rmd", "w")
  writeLines("```{r}", con = report)

  # load pkgs
  catf("library(%s)\n",needed.pkgs, file = report)

  # load data
  data.path = file.path(x$corr.obj$task$data.path, x$corr.obj$task$data.name)
  catf("%s = readRDS(\"%s.rds\")", x$corr.obj$task$data.name, data.path, file = report)

  #save object
  obj.name = deparse(substitute(x))
  obj.file.name = paste0(obj.name, ".rds")
  saveRDS(x, file = obj.file.name)

  #load object; x$var.id is needed so the plotting code refernce the right objects
  obj.path = file.path(getwd(), obj.file.name)
  catf("%s = readRDS(\"%s\")", x$var.id , obj.path, file = report)

  writeLines("```", con = report)
  writeLines("SOme text; CorrPlot ....", con = report)
  writeLines("```{r}", con = report)
  writeLines(x$plot.code$code, con = report)
  writeLines("```", con = report)
  close(report)
  setwd(origin.wd)
}

### Example
# saveRDS(cars, file="cars.rds")
# my.task = makeCorrTask(id = "test", data = cars)
# my.corr = makeCorr(my.task)
# report1 = makeCorrReport(my.corr, type = "CorrPlot")
# writeReport(report1)
#
# data(diamonds, package = "ggplot2")
# saveRDS(diamonds, file="diamonds.rds")
# my.task = makeCorrTask(id = "test", data = diamonds)
# my.corr = makeCorr(my.task)
# report1 = makeCorrReport(my.corr, type = "CorrPlot")
# writeReport(report1)
