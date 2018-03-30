context("Numeric Summar Report")
boston = data("Boston", package = "MASS")
boston = get(boston)[, c("crim", "zn", "indus", "medv")]

test_that("makeNumSumTask", {
  num.sum.task = makeNumSumTask(id = "test.report", data = boston, target = "medv")

  expectIdentical(num.sum.task$size, nrow(boston))
  expectIdentical(num.sum.task$env$data, boston)
  expect_class(num.sum.task, "NumSumTask")
  expectIdentical(num.sum.task$id, "test.report")
  expectIdentical(names(num.sum.task$numdatatypes), c("numeric", "integer"))
  expectIdentical(num.sum.task$geom.hist.args, list(bins = 30, alpha = 0.4))
  expectIdentical(num.sum.task$geom.dens.args, list(size = 2, alpha = 0.4))
  expectIdentical(num.sum.task$geom.box.args, list())

  num.sum.task = makeNumSumTask(id = "test.report", data = boston, target = NULL)
})

test_that("makeNumSum", {
  num.sum.task = makeNumSumTask(id = "test.report", data = boston, target = "medv")
  num.sum = makeNumSum(num.sum.task)

  expect_class(num.sum, "NumSumObj")
  expectIdentical(num.sum$task, num.sum.task)
  expectIdentical(length(num.sum), 3L)
  expectIdentical(length(num.sum$num.sum), 4L)
  expect_matrix(num.sum$num.sum.df, nrows = 4, ncols = 25L)
})

test_that("makeNumSumReport", {
  num.sum.task = makeNumSumTask(id = "test.report", data = boston, target = "medv")
  num.sum = makeNumSum(num.sum.task)
  num.sum.report = makeNumSumReport(num.sum)

  expect_class(num.sum.report, "NumSumReport")
  expectIdentical(num.sum.report$report.task, num.sum.task)
  expectIdentical(length(num.sum.report), 5L)
  expectIdentical(num.sum.report$type, "NumericReport")
  expect_character(num.sum.report$report.id)
  expectIdentical(num.sum.report$num.sum.df, num.sum$num.sum.df)
  expect_list(num.sum.report$num.sum.var, len = 4L)
})

test_that("writeNumSumReport", {
  num.sum.task = makeNumSumTask(id = "test.report", data = boston, target = "medv")
  num.sum = makeNumSum(num.sum.task)
  num.sum.report = makeNumSumReport(num.sum)

  temp.wd = getwd()
  expect_file((rmd.file = writeReport(num.sum.report, save.mode = FALSE, override = TRUE)), extension = "rmd")
  expect_file(x = paste0("Data_Report/", num.sum.report$report.id, ".rds"))
  expectIdentical(getwd(), temp.wd)
  rds.obj = readRDS(paste0("Data_Report/", num.sum.report$report.id, ".rds"))
  lapply(seq_along(rds.obj$num.sum.var), FUN = function(i) {
    expectIdentical(rds.obj$num.sum.var[[i]]$summary, num.sum.report$num.sum.var[[i]]$summary)
  })
  expect_error(writeReport(num.sum.report))
  expectIdentical(getwd(), temp.wd)

  finishReport(num.sum.report, save.mode = FALSE, override = TRUE)
  rmarkdown::render("MainReport.rmd", quiet = TRUE)
  unlink("Data_Report", recursive = TRUE)
})

