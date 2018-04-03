context("Factor Analysis")
suppressWarnings(library(vegan))
data(bfi)

set.seed(1)
bfi.small = bfi[sample(seq_len(nrow(bfi)), size = 200L), ]
fa.task = makeFATask(id = "bfi", data = bfi_small)
fa.res = makeFA(fa.task)
fa.report = makeFAReport(fa.res)
finishReport(fa.report)

test_that("makeFATask", {
  #wrong par.vals
  expect_message({makeFATask(id = "bfi", data = bfi_small, show.NA.msg = TRUE)})
  expect_error({makeFATask(id = "bfi", data = bfi.small,
    par.vals = list(nfact = 7))})
  expect_error({makeFATask(id = "bfi", data = bfi.small,
    par.vals = list(imputation = "mean"))})
  expect_list(fa.task$numdatatypes, len = 2L)
})

test_that("makeFA", {
  expect_list(fa.res$fa.result, len = 51L)
  expect_vector(fa.res$fa.result$communalities, len = 28L)
  expect_matrix(fa.res$fa.result$r, nrows = 28L, ncols = 28L)
})

test_that("makeFAReport", {
  expect_class(fa.report, "FAReport")
  expect_identical(length(fa.report), 4L)
  expect_list(fa.report$task, len = 9L)
  expect_list(fa.report$fa.result, len = 51L)
  expect_character(fa.report$report.id)
  expect_character(fa.report$type)
})

test_that("writeFAReport", {
  temp.wd = getwd()
  expect_file((rmd.file = writeReport(fa.report, save.mode = FALSE,
    override = TRUE)), extension = "rmd")
  expect_file(x = paste0("Data_Report/", fa.report$report.id, ".rds"))
  expect_identical(getwd(), temp.wd)
  rds.obj = readRDS(paste0("Data_Report/", fa.report$report.id, ".rds"))
  expect_equal(rds.obj$task, fa.report$task)
  expect_identical(rds.obj$report.id, rds.obj$report.id)
  expect_equal(class(rds.obj), class(fa.report))
  expect_error(writeReport(fa.report))
  expect_identical(getwd(), temp.wd)

  setwd(paste0(temp.wd, "/Data_Report"))
  knitr::knit2html(gsub("Data_Report/", "", rmd.file), quiet = TRUE)
  setwd(temp.wd)
  unlink("Data_Report", recursive = TRUE)
})
