context("Plotting Funktions")
library(ggplot2)
data("airquality")
set.seed(1L)
ind = sample.int(153, 10)
airquality = airquality[ind, ]
airquality$Nominal = factor(c(rep("A", 3), rep("B", 7)))
gg1 = ggplot(data=airquality, aes(x=Temp, y=Wind)) + geom_point()
gg2 = ggplot(data=airquality, aes(x=Day, y=Wind)) + geom_point()
gg3 = ggplot(data=airquality, aes(x=Temp, y=Solar.R)) + geom_point()
gg4 = ggplot(data=airquality, aes(x=Temp, y=Ozone)) + geom_point()
test_that("multiplot", {
  multiplot(gg1, gg2, gg3, gg4, layout = matrix(c(1,2,3,4), nrow=2, byrow=TRUE))
  expect_error(multiplot(gg1, gg2, gg3, gg4, layout = matrix(c(1,2,3,3), nrow=2, byrow=TRUE)))
  multiplot(gg1, gg2, plotlist = list(gg3, gg4))
  multiplot(gg1, gg2, cols = 2)

})

test_that("multiplotPages", {
  multiplotPages(list(gg1, gg2, gg3, gg4), k=2, 2)
})

test_that("multiplotPages", {
  plotBar(airquality, target = "Nominal")
  expect_error(plotBar(airquality, target = "Temp"))
})
