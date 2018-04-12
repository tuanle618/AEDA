context("Categorial Data Summary")
data("Arthritis", package = "vcd")
test_that("makeCatSumTask", {
  cat.sum.task = makeCatSumTask(id = "test.report", data = Arthritis, target = "Improved")
  expectIdentical(cat.sum.task$id, "test.report")
  expectIdentical(cat.sum.task$type, "CategoricalSummary")
  expectIdentical(cat.sum.task$env$data, Arthritis)
  expectIdentical(cat.sum.task$size, nrow(Arthritis))
  expectIdentical(cat.sum.task$catdatatypes$factor, c("Treatment", "Sex"))
  expectIdentical(cat.sum.task$catdatatypes$ordered, "Improved")
  expectIdentical(cat.sum.task$catdatatypes$logical, character(0))
  expectIdentical(cat.sum.task$geombar.args, list())
  expect_class(cat.sum.task, "CatSumTask")

  cat.sum.task = makeCatSumTask(id = "test.report", data = Arthritis, target = "Improved", position = "dodge")
  expectIdentical(cat.sum.task$geombar.args, list(position = "dodge"))
  expect_error(makeCatSumTask(id = "test.report", data = Arthritis))
  expect_error(makeCatSumTask(data = Arthritis, target = "Wind"))
})

test_that("makeCatSum", {
  cat.sum.task = makeCatSumTask(id = "test.report",
    data = Arthritis, target = "Improved")
  cat.sum = makeCatSum(cat.sum.task)

  expect_class(cat.sum, "CatSumObj")
  expectIdentical(cat.sum$task, cat.sum.task)
  expectIdentical(length(cat.sum), 2L)
  expectIdentical(length(cat.sum$cat.sum), 6L)
})

test_that("makeCatSumReport", {
  cat.sum.task = makeCatSumTask(id = "test.report",
    data = Arthritis, target = "Improved")
  cat.sum = makeCatSum(cat.sum.task)
  cat.sum.report = makeReport(cat.sum)

  expect_class(cat.sum.report, "CatSumReport")
  expectIdentical(cat.sum.report$task, cat.sum.task)
  expectIdentical(length(cat.sum.report), 4L)
  expectIdentical(cat.sum.report$type, "CategoricalReport")
  expect_character(cat.sum.report$report.id)
})

test_that("writeCatSumReport", {
  cat.sum.task = makeCatSumTask(id = "test.report",
    data = Arthritis, target = "Improved")
  cat.sum = makeCatSum(cat.sum.task)
  cat.sum.report = makeReport(cat.sum)

  temp.wd = getwd()
  expect_file((rmd.file = writeReport(cat.sum.report,
    save.mode = FALSE, override = TRUE)), extension = "rmd")
  expect_file(x = paste0("Data_Report/", cat.sum.report$report.id, ".rds"))
  expectIdentical(getwd(), temp.wd)
  rds.obj = readRDS(paste0("Data_Report/", cat.sum.report$report.id, ".rds"))
  expectIdentical(rds.obj$cat.sum[!names(rds.obj$cat.sum) %in% "plot.list"], cat.sum.report$cat.sum[!names(cat.sum.report$cat.sum) %in% "plot.list"])

  expect_error(writeReport(cat.sum.report))
  expectIdentical(getwd(), temp.wd)

  setwd(paste0(temp.wd, "/Data_Report"))
  knitr::knit2html(gsub("Data_Report/", "", rmd.file), quiet = TRUE)
  setwd(temp.wd)
  unlink("Data_Report", recursive = TRUE)
})
