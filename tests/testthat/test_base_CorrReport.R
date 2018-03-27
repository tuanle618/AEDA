context("Correlation Analysis")
data("Boston", package = "MASS")
corr.task = makeCorrTask(id = "test.report", data = Boston)
test_that("makeCorrTask", {
  expect_identical(corr.task$id, "test.report")
  expect_identical(corr.task$type, "CorrPlot")
  expect_identical(corr.task$env$data, Boston)
  expect_identical(corr.task$size, nrow(Boston))
  expect_identical(corr.task$features$num, c("crim", "zn", "indus",
    "nox", "rm", "age", "dis", "tax", "ptratio", "black", "lstat",
    "medv"))
  expect_identical(corr.task$features$int, c("chas", "rad"))
  expect_identical(corr.task$method, "pearson")
  expect_class(corr.task, "CorrTask")
  expect_identical(corr.task$data.name, "Boston")
  expect_null(corr.task$needed.pkgs)
  expect_identical(corr.task$missing.values, 0L)

  corr.task.2 = makeCorrTask(id = "test.report",
    data = Boston, method = "spearman")
  expect_identical(corr.task.2$method, "spearman")
})

corr = makeCorr(corr.task)
test_that("makeCorr", {
  expect_class(corr, "CorrObj")
  expect_equal(corr$corr.matrix,
    cor(x = corr$env$data[, unlist(corr$features)],
      method = corr.task$method))
  expect_identical(length(corr), 10L)
  expect_identical(dim(corr$corr.matrix), c(14L, 14L))
})

corr.report = makeReport(corr)
test_that("makeClusterAnalysisReport", {
  expect_class(corr.report, "CorrReport")
  expect_identical(length(corr.report), 12L)
  expect_character(corr.report$report.id)
  expect_character(corr.report$plot.code$code)
  expect_character(corr.report$plot.code$needed.pkg)
})

test_that("writeClusterAnalysisReport", {
  temp.wd = getwd()
  expect_file((rmd.file = writeReport(corr.report, save.mode = FALSE, override = TRUE)), extension = "rmd")
  expect_file(x = paste0("Data_Report/", corr.report$report.id, ".rds"))
  expect_identical(getwd(), temp.wd)
  rds.obj = readRDS(paste0("Data_Report/", corr.report$report.id, ".rds"))
  expect_identical(rds.obj$corr.matrix, corr.report$corr.matrix)
  expect_identical(rds.obj$method, corr.report$method)

  expect_error(writeReport(corr.report))
  expect_identical(getwd(), temp.wd)

  setwd(paste0(temp.wd, "/Data_Report"))
  knitr::knit2html(gsub("Data_Report/", "", rmd.file), quiet = TRUE)
  setwd(temp.wd)
  unlink("Data_Report", recursive = TRUE)
})
