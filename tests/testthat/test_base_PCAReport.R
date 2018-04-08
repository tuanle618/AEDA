context("Principal Component Analysis")
data("cars")
data("iris")
suppressWarnings(library(MASS))


pca.task = makePCATask(id = "iris.test", data = iris, target = "Species")
pca.result = makePCA(pca.task)
pca.report = makePCAReport(pca.result)
pca.report.s3 = makeReport(pca.result)

test_that("makePCATask", {
  #error if no target is specified
  expect_error({makePCATask(id = "cars.fail", data = cars)})
  #insert data frame which only has 2 numeric columns
  expect_error({makePCATask(id = "cars.fail", data = cars, target = NULL)})
  #check correct output
  expect_class(pca.task, "PCATask")
  expect_vector(pca.task$features, len = 4L)
})

test_that("makePCA", {
  #wrong parameters handed over to prcomp
  pca.fail.task = makePCATask(id = "iris.test", data = iris, target = "Species",
    cent = TRUE, tolerance = 0.01)
  expect_error({makePCA(pca.fa)})
  #check outout for correct computations
  expect_list(pca.result, len = 3L)
  expect_class(pca.result, "PCAObj")
  expect_matrix(pca.result$pca.result$rotation, ncols = 4L, nrows = 4L)
  expect_numeric(pca.result$pca.result$sdev, len = 4L)
  expect_list(pca.result$plotlist, len = 4L)
  expect_identical(class(pca.result$plotlist$pca.scree), c("gg", "ggplot"))
  expect_identical(class(pca.result$plotlist$pca.scatter.1), c("gg", "ggplot"))
  expect_identical(class(pca.result$plotlist$pca.scatter.named), c("gg", "ggplot"))
  expect_identical(class(pca.result$plotlist$pca.scatter.indidivuals), c("gg", "ggplot"))
})

test_that("makePCAReport", {
  expect_class(pca.report, "PCAReport")
  expect_identical(length(pca.report), 4L)
  expect_list(pca.report$pca.result, len = 3L)
  expect_character(pca.report$report.id)
  expect_character(pca.report$type)

  expect_class(pca.report.s3, "PCAReport")
  expect_identical(length(pca.report.s3), 4L)
  expect_list(pca.report.s3$pca.result, len = 3L)
  expect_character(pca.report.s3$report.id)
  expect_character(pca.report.s3$type)
})

test_that("writePCAReport", {
  temp.wd = getwd()
  expect_file((rmd.file = writeReport(pca.report, save.mode = FALSE,
    override = TRUE)), extension = "rmd")
  expect_file(x = paste0("Data_Report/", pca.report$report.id, ".rds"))
  expect_identical(getwd(), temp.wd)
  rds.obj = readRDS(paste0("Data_Report/", pca.report$report.id, ".rds"))
  expect_equal(rds.obj$task, pca.report$task)
  expect_identical(pca.report$report.id, rds.obj$report.id)
  expect_equal(class(rds.obj), class(pca.report))
  expect_error(writeReport(pca.report))
  expect_identical(getwd(), temp.wd)

  setwd(paste0(temp.wd, "/Data_Report"))
  knitr::knit2html(gsub("Data_Report/", "", rmd.file), quiet = TRUE)
  setwd(temp.wd)
  unlink("Data_Report", recursive = TRUE)
})
