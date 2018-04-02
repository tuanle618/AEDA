context("getNumSum")
data("airquality")
set.seed(1L)
airquality$Nominal = as.factor(sample(c("A", "B"), 153, replace = TRUE))

test_that("getNumSum", {
  expect_warning({num.sum = getNumSum(data = airquality, target = "Nominal",
    c("Temp", "Ozone", "Solar.R"),  geom.hist.args = list(bins = 30, alpha = 0.4), geom.dens.args = list(size = 2, alpha = 0.4), geom.box.args = list())})
  expect_list(num.sum, len = 2L)
  expect_matrix(num.sum$num.sum.df, nrows = 3L, ncols = 25L)
  expect_list(num.sum$merged.list, len = 3L)
  lapply(num.sum$merged.list, FUN = function(x){
    expect_equal(length(x), 4L)
    expect_equal(class(x[[2]]), c("gg", "ggplot"))
    expect_equal(class(x[[3]]), c("gg", "ggplot"))
    expect_equal(class(x[[4]]), c("gg", "ggplot"))
  })
})
