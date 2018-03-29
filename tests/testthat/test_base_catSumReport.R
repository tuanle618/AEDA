context("Categorial Data Summary")
data("Arthritis", package = "vcd")
test_that("makeCatSumTask", {
  cat.sum.task = makeCatSumTask(id = "test.report", data = Arthritis, target = "Improved")
  expect_identical(cat.sum.task$id, "test.report")
  expect_identical(cat.sum.task$type, "CategoricalSummary")
  expect_identical(cat.sum.task$env$data, Arthritis)
  expect_identical(cat.sum.task$size, nrow(Arthritis))
  expect_identical(cat.sum.task$catdatatypes$factor, c("Treatment", "Sex"))
  expect_identical(cat.sum.task$catdatatypes$ordered, "Improved")
  expect_identical(cat.sum.task$catdatatypes$logical, character(0))
  expect_identical(cat.sum.task$geombar.args, list())
  expect_class(cat.sum.task, "CatSumTask")

  cat.sum.task = makeCatSumTask(id = "test.report", data = Arthritis,
    target = "Improved", position = "dodge")
  expect_identical(cat.sum.task$geombar.args, list(position = "dodge"))
  expect_error(makeCatSumTask(id = "test.report", data = Arthritis))
  expect_error(makeCatSumTask(data = Arthritis, target = "Wind"))
})

test_that("makeCatSum", {
  cat.sum.task = makeCatSumTask(id = "test.report", data = Arthritis, target = "Improved")
  cat.sum = makeCatSum(cat.sum.task)

  expect_class(cat.sum, "CatSumObj")
  expect_identical(cat.sum$task, cat.sum.task)
  expect_identical(length(cat.sum), 2L)
  expect_identical(length(cat.sum$cat.sum), 6L)
})

test_that("makeCatSumReport", {
  cat.sum.task = makeCatSumTask(id = "test.report", data = Arthritis, target = "Improved")
  cat.sum = makeCatSum(cat.sum.task)
  cat.sum.report = makeCatSumReport(cat.sum)

  expect_class(cat.sum.report, "CatSumReport")
  expect_identical(cat.sum.report$report.task, cat.sum.task)
  expect_identical(length(cat.sum.report), 4L)
  expect_identical(cat.sum.report$type, "CategoricalReport")
  expect_character(cat.sum.report$report.id)
})

test_that("writeCatSumReport", {
  cat.sum.task = makeCatSumTask(id = "test.report", data = Arthritis, target = "Improved")
  cat.sum = makeCatSum(cat.sum.task)
  cat.sum.report = makeCatSumReport(cat.sum)

  temp.wd = getwd()
  expect_file((rmd.file = writeReport(cat.sum.report, save.mode = FALSE, override = TRUE)), extension = "rmd")
  expect_file(x = paste0("Data_Report/", cat.sum.report$report.id, ".rds"))
  expect_identical(getwd(), temp.wd)
  rds.obj = readRDS(paste0("Data_Report/", cat.sum.report$report.id, ".rds"))
  expect_identical(rds.obj$cat.sum[!names(rds.obj$cat.sum) %in% "plot.list"],
    cat.sum.report$cat.sum[!names(cat.sum.report$cat.sum) %in% "plot.list"])

  expect_error(writeReport(cat.sum.report))
  expect_identical(getwd(), temp.wd)

  setwd(paste0(temp.wd, "/Data_Report"))
  knitr::knit2html(gsub("Data_Report/", "", rmd.file), quiet = TRUE)
  setwd(temp.wd)
  unlink("Data_Report", recursive = TRUE)
})
