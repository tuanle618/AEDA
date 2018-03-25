context("Numeric Summar Report")
boston = data("Boston", package = "MASS")
boston = get(boston)[, 1:3]

test_that("makeNumSumTask", {
  num.sum.task = makeNumSumTask(id = "test.report", data = boston, target = "medv")

  expect_identical(num.sum.task$size, nrow(boston))
  expect_identical(num.sum.task$env$data, boston)
  expect_class(num.sum.task, "NumSumTask")
  expect_identical(num.sum.task$id, "test.report")
  expect_identical(names(num.sum.task$numdatatypes), c("numeric", "integer"))
  expect_identical(num.sum.task$geom.hist.args, list(bins = 30, alpha = 0.4))
  expect_identical(num.sum.task$geom.dens.args, list(size = 2, alpha = 0.4))
  expect_identical(num.sum.task$geom.box.args, list())

  num.sum.task = makeNumSumTask(id = "test.report", data = boston, target = NULL)
})

test_that("makeNumSum", {
  num.sum.task = makeNumSumTask(id = "test.report", data = boston, target = "medv")
  num.sum = makeNumSum(num.sum.task)

  expect_class(num.sum, "NumSumObj")
  expect_identical(num.sum$task, num.sum.task)
  expect_identical(length(num.sum), 3L)
  expect_identical(length(num.sum$num.sum), 3L)
  expect_matrix(num.sum$num.sum.df, nrows = 3, ncols = 25L)
})

test_that("makeNumSumReport", {
  num.sum.task = makeNumSumTask(id = "test.report", data = boston, target = "medv")
  num.sum = makeNumSum(num.sum.task)
  num.sum.report = makeNumSumReport(num.sum)

  expect_class(num.sum.report, "NumSumReport")
  expect_identical(num.sum.report$report.task, num.sum.task)
  expect_identical(length(num.sum.report), 5L)
  expect_identical(num.sum.report$type, "NumericReport")
  expect_character(num.sum.report$report.id)
  expect_identical(num.sum.report$num.sum.df, num.sum$num.sum.df)
  expect_list(num.sum.report$num.sum.var, len = 3L)
})

test_that("writeNumSumReport", {
  num.sum.task = makeNumSumTask(id = "test.report", data = boston, target = "medv")
  num.sum = makeNumSum(num.sum.task)
  num.sum.report = makeNumSumReport(num.sum)

  temp.wd = getwd()
  expect_file((rmd.file = writeReport(num.sum.report, save.mode = FALSE, override = TRUE)), extension = "rmd")
  expect_file(x = paste0("Data_Report/", num.sum.report$report.id, ".rds"))
  expect_identical(getwd(), temp.wd)
  rds.obj = readRDS(paste0("Data_Report/", num.sum.report$report.id, ".rds"))
  lapply(seq_along(rds.obj$num.sum.var), FUN = function(i) {
    expect_identical(rds.obj$num.sum.var[[i]]$summary, num.sum.report$num.sum.var[[i]]$summary)
  })
  expect_error(writeReport(num.sum.report))
  expect_identical(getwd(), temp.wd)

  finishReport(num.sum.report, save.mode = FALSE, override = TRUE)
  rmarkdown::render("MainReport.rmd", quiet = TRUE)
  unlink("Data_Report", recursive = TRUE)
})

