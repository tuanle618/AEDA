context("finishReport")
library("MASS")

test_that("finishReport Works",{
  data("Boston", package = "MASS")
  temp.wd = getwd()
  dir.create("TestingDir")
  setwd("TestingDir")
  # Test with Boston dataset and just two reports
  num.sum.task = makeNumSumTask(id = "BostonTask", data = Boston, target = "medv")
  num.sum = makeNumSum(num.sum.task)
  num.sum.report = makeNumSumReport(num.sum)
  my.task = makeCorrTask(id = "test", data = Boston, type = "CorrPlot")
  my.corr = makeCorr(my.task)
  report1 = makeCorrReport(my.corr)
    data(diamonds, package = "ggplot2")
  my.task = makeCorrTask(id = "test2", data = diamonds, type = "CorrPlot")
  my.corr = makeCorr(my.task)
  report2 = makeCorrReport(my.corr)
    data("Arthritis", package = "vcd")
  cat.sum.task = makeCatSumTask(id = "Arthritis.Task", data = Arthritis,
   target = "Improved", na.rm = TRUE)
  cat.sum = makeCatSum(cat.sum.task)
  cat.sum.report = makeCatSumReport(cat.sum)
  #combine all reports
  finishReport(basic.report, num.sum.report, report1, report2,
   cat.sum.report, save.mode = FALSE, override = TRUE)



  # Clean up
  setwd(temp.wd)
  unlink("TestingDir", recursive = TRUE)
})
