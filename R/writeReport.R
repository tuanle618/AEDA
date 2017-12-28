#' Writes and rmd file for the report WIP
#' @param x
#' @param dir
#'   directory where the sub directory is created
#'
writeReport = function(x, sub.dir = "Data_Report"){
  # Create sub directory, save current wd and set new wd to the new directory
  origin.wd = createDir(sub.dir)

  # TryCatch sets wd back and closes all open connections if an error occurs
  tryCatch(
      {
      # Collect all needed packages
      needed.pkgs = getPkgs(x)

      #start the report file
      report <- file("report.rmd", "w")
      writeLines("```{r}", con = report)

      # load pkgs
      rmdLibrary(needed.pkgs, report)

      # load data
      data.path = file.path(x$data.path, x$data.name)
      rmdloadData(x$data.name, data.path, report)

      #save object
      obj.name = deparse(substitute(x))
      obj.file.name = paste0(obj.name, ".rds")
      saveRDS(x, file = obj.file.name)
      #load object; x$var.id is needed so the plotting code refernce the right objects
      rmdloadData(x$report.id, obj.name, report)

      writeLines("```", con = report)
      writeLines("Some text; CorrPlot ....", con = report)
      writeLines("```{r}", con = report)
      rmdWriteLines(x$plot.code$code, con = report)
      writeLines("```", con = report)
      close(report)
      setwd(origin.wd)
      },
    finally = function(){
      setwd(origin.wd)
      closeAllConnections()
    }
  )
}

### Example
saveRDS(cars, file="Data_Report/cars.rds")
my.task = makeCorrTask(id = "test", data = cars)
my.corr = makeCorr(my.task)
report1 = makeCorrReport(my.corr, type = "CorrPlot")
writeReport(report1)
#
# data(diamonds, package = "ggplot2")
# saveRDS(diamonds, file="diamonds.rds")
# my.task = makeCorrTask(id = "test", data = diamonds)
# my.corr = makeCorr(my.task)
# report1 = makeCorrReport(my.corr, type = "CorrPlot")
# writeReport(report1)
