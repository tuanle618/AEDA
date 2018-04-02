context("Correlation Analysis")
data("Boston", package = "MASS")
corr.task = makeCorrTask(id = "test.report", data = Boston)
test_that("makeCorrTask", {
  expectIdentical(corr.task$id, "test.report")
  expectIdentical(corr.task$type, "CorrPlot")
  expectIdentical(corr.task$env$data, Boston)
  expectIdentical(corr.task$size, nrow(Boston))
  expectIdentical(corr.task$features$num, c("crim", "zn", "indus",
    "nox", "rm", "age", "dis", "tax", "ptratio", "black", "lstat",
    "medv"))
  expectIdentical(corr.task$features$int, c("chas", "rad"))
  expectIdentical(corr.task$method, "pearson")
  expect_class(corr.task, "CorrTask")
  expectIdentical(corr.task$data.name, "Boston")
  expect_null(corr.task$needed.pkgs)
  expectIdentical(corr.task$missing.values, 0L)

  corr.task.2 = makeCorrTask(id = "test.report",
    data = Boston, method = "spearman")
  expectIdentical(corr.task.2$method, "spearman")
})

corr = makeCorr(corr.task)
test_that("makeCorr", {
  expect_class(corr, "CorrObj")
  expect_equal(corr$corr.matrix,
    cor(x = corr$env$data[, unlist(corr$features)],
      method = corr.task$method))
  expectIdentical(length(corr), 10L)
  expectIdentical(dim(corr$corr.matrix), c(14L, 14L))
})

corr.report = makeReport(corr)
test_that("makeClusterAnalysisReport", {
  expect_class(corr.report, "CorrReport")
  expectIdentical(length(corr.report), 12L)
  expect_character(corr.report$report.id)
  expect_character(corr.report$plot.code$code)
  expect_character(corr.report$plot.code$needed.pkg)
})

test_that("writeClusterAnalysisReport", {
  temp.wd = getwd()
  expect_file((rmd.file = writeReport(corr.report, save.mode = FALSE, override = TRUE)), extension = "rmd")
  expect_file(x = paste0("Data_Report/", corr.report$report.id, ".rds"))
  expectIdentical(getwd(), temp.wd)
  rds.obj = readRDS(paste0("Data_Report/", corr.report$report.id, ".rds"))
  expectIdentical(rds.obj$corr.matrix, corr.report$corr.matrix)
  expectIdentical(rds.obj$method, corr.report$method)

  expect_error(writeReport(corr.report))
  expectIdentical(getwd(), temp.wd)

  setwd(paste0(temp.wd, "/Data_Report"))
  knitr::knit2html(gsub("Data_Report/", "", rmd.file), quiet = TRUE)
  setwd(temp.wd)
  unlink("Data_Report", recursive = TRUE)
})
