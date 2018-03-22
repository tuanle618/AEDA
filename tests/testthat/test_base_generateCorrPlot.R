context("genaretCorrPlot")
data("airquality")

test_that("genrateCorrPlot",{
  my.task = makeCorrTask(id = "test", data = airquality)
  my.corr = makeCorr(my.task)
  corr.plot = AEDA:::generateCorrPlot(my.corr, "my.corr")
  expect_character(corr.plot$needed.pkg)
  expect_character(corr.plot$code)
  library(corr.plot$needed.pkg, character.only=TRUE)
  eval(parse(text=corr.plot$code))
})
