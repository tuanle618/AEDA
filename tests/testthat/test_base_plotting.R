context("Plotting Functions")
library(ggplot2)
data("airquality")
set.seed(1L)
ind = sample.int(153, 10)
airquality = airquality[ind, ]
airquality$Nominal = factor(c(rep("A", 3), rep("B", 7)))
gg1 = ggplot(data = airquality, aes(x = Temp, y = Wind)) + geom_point()
gg2 = ggplot(data = airquality, aes(x = Day, y = Wind)) + geom_point()
gg3 = ggplot(data = airquality, aes(x = Temp, y = Solar.R)) + geom_point()
gg4 = ggplot(data = airquality, aes(x = Temp, y = Ozone)) + geom_point()
test_that("multiplot", {
  suppressWarnings({
    multiplot(gg1, gg2, gg3, gg4, layout = matrix(c(1, 2, 3, 4), nrow = 2, byrow = TRUE))
    expect_error(multiplot(gg1, gg2, gg3, gg4, layout = matrix(c(1, 2, 3, 3), nrow = 2, byrow = TRUE)))
    multiplot(gg1, gg2, plotlist = list(gg3, gg4))
    multiplot(gg1, gg2, cols = 2)
    expect_error(multiplot(gg1, "gg2", cols = 2))
    expect_error(multiplot(plotlist = list(gg3, "gg4"), cols = 2))
  })
})

test_that("multiplotPages", {
  suppressWarnings(multiplotPages(list(gg1, gg2, gg3, gg4), k = 2, 2))
  expect_error(multiplotPages(list(gg1, gg2, gg3, "gg4"), k = 2, 2))
})

test_that("plotBar", {
  plotBar(airquality, target = "Nominal")
  expect_error(plotBar(airquality, target = "Temp"))
  expect_error(plotBar(airquality, target = "NoFeature"))
})

test_that("plotBox", {
  plotBox(airquality, target = "Nominal")
  plotBox(airquality, target = NULL)
  plotBox(airquality, target = "Nominal", plot.x.only = TRUE)
  plotBox(airquality, target = NULL, col = "Temp")
  expect_error(plotBox(airquality, target = "Temp"))
  expect_error(plotBox(airquality, target = "NoFeature"))

})

test_that("plotDens", {
  plotDens(airquality, target = NULL)
  plotDens(airquality, target = "Nominal")
  plotDens(airquality, target = "Temp")
  expect_error(plotDens(airquality, target = "NoFeature"))

})

test_that("plotFeatDistr", {
  plotFeatDistr(airquality, target = NULL, col = "Temp")
  plotFeatDistr(airquality, target = "Nominal", col = "Temp")
  expect_error(plotFeatDistr(airquality, target = "Ozone", col = "Temp"))
  expect_error(plotFeatDistr(airquality, target = "NoFeature", col = "Temp"))
})

test_that("plotHist", {
  plotHist(airquality, target = NULL, col = "Temp")
  plotHist(airquality, target = "Nominal", col = "Temp")
  expect_error(plotHist(airquality, target = "Ozone", col = "Temp"))
  expect_error(plotHist(airquality, target = "NoFeature", col = "Temp"))
})
