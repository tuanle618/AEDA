context("MultiDimensional Scaling Analysis")
data("swiss")
data("iris")
suppressWarnings(library(MASS))
suppressWarnings(library(smacof))
suppressWarnings(library(vegan))

mds.task = makeMDSTask(id = "swiss", data = swiss, target = "Infant.Mortality")
mds.analysis = getMDSAnalysis(dist = mds.task$dist, method = mds.task$method,
  par.vals = mds.task$par.vals)

test_that("makeMDSTask", {
  #wrong norms
  expect_warning({makeMDSTask(id = "iris", data = iris, target = "Species")})
  expect_error({makeMDSTask(id = "swiss", data = swiss, target = "Infant.Mortality",
    dist.norm = "L2")})
  expect_error({makeMDSTask(id = "swiss", data = swiss, target = "Infant.Mortality",
    dist.norm = "L1")})
  #wrong methods
  expect_error({makeMDSTask(id = "swiss", data = swiss, target = "Infant.Mortality",
    method = "isomds")})
  #wrong par.vals
  expect_error({makeMDSTask(id = "swiss", data = swiss, target = "Infant.Mortality",
    method = "cmdscale", par.vals = list(eigen = TRUE))})
  expect_error({makeMDSTask(id = "swiss", data = swiss, target = "Infant.Mortality",
    method = "wcmdscale", par.vals = list(eig = TRUE,
      weights = seq(0, 1, nrow(swiss))))})
  expect_error({makeMDSTask(id = "swiss", data = swiss, target = "Infant.Mortality",
    method = "smacofSym", par.vals = list(k = 2, itermax = 100L))})
  expect_error({makeMDSTask(id = "swiss", data = swiss, target = "Infant.Mortality",
    method = "isoMDS", par.vals = list(itermax = 100L))})
  expect_error({makeMDSTask(id = "swiss", data = swiss, target = "Infant.Mortality",
    method = "sammon", par.vals = list(itermax = 100L))})
  #check correct output
  expect_class(mds.task$dist, "dist")
  expect_list(mds.task$numdatatypes, len = 2L)
})

test_that("getMDSAnalysis cmdscale", {
  mds.task = makeMDSTask(id = "swiss", data = swiss, target = "Infant.Mortality")
  mds.analysis = getMDSAnalysis(mds.task$dist, method = mds.task$method,
    par.vals = mds.task$par.vals)
  expect_matrix(mds.analysis$mds.result, ncols = 2L, nrows = 47L)
  expect_data_frame(mds.analysis$mds.result.data, ncols = 2, nrows = 47L)
  expect_identical(class(mds.analysis$mds.plot), c("gg", "ggplot"))
})

test_that("getMDSAnalysis wcmdscale", {
  mds.task = makeMDSTask(id = "swiss", data = swiss, target = "Infant.Mortality",
    method = "wcmdscale")
  mds.analysis = getMDSAnalysis(mds.task$dist, method = mds.task$method,
    par.vals = mds.task$par.vals)
  expect_matrix(mds.analysis$mds.result, ncols = 2L, nrows = 47L)
  expect_data_frame(mds.analysis$mds.result.data, ncols = 2L, nrows = 47L)
  expect_identical(class(mds.analysis$mds.plot), c("gg", "ggplot"))
})

test_that("getMDSAnalysis smacofSym", {
  mds.task = makeMDSTask(id = "swiss", data = swiss, target = "Infant.Mortality",
    method = "smacofSym")
  mds.analysis = getMDSAnalysis(mds.task$dist, method = mds.task$method,
    par.vals = mds.task$par.vals)
  expect_list(mds.analysis$mds.result, len = 17L)
  expect_identical(class(mds.analysis$mds.result), c("smacofB", "smacof"))
  expect_data_frame(mds.analysis$mds.result.data, ncols = 2L, nrows = 47L)
  expect_identical(class(mds.analysis$mds.plot), c("gg", "ggplot"))
})

test_that("getMDSAnalysis isoMDS", {
  mds.task = makeMDSTask(id = "swiss", data = swiss, target = "Infant.Mortality",
    method = "isoMDS", par.vals = list(trace = FALSE))
  mds.analysis = getMDSAnalysis(mds.task$dist, method = mds.task$method,
    par.vals = mds.task$par.vals)
  expect_list(mds.analysis$mds.result, len = 2L)
  expect_identical(class(mds.analysis$mds.result[[1]]), "matrix")
  expect_data_frame(mds.analysis$mds.result.data, ncols = 2, nrows = 47L)
  expect_identical(class(mds.analysis$mds.plot), c("gg", "ggplot"))
})

test_that("getMDSAnalysis sammon", {
  mds.task = makeMDSTask(id = "swiss", data = swiss, target = "Infant.Mortality",
    method = "sammon", par.vals = list(trace = FALSE))
  mds.analysis = getMDSAnalysis(mds.task$dist, method = mds.task$method,
    par.vals = mds.task$par.vals)
  expect_list(mds.analysis$mds.result, len = 3L)
  expect_identical(class(mds.analysis$mds.result[[1]]), "matrix")
  expect_data_frame(mds.analysis$mds.result.data, ncols = 2, nrows = 47L)
  expect_identical(class(mds.analysis$mds.plot), c("gg", "ggplot"))
})

mds.task = makeMDSTask(id = "swiss", data = swiss, target = "Infant.Mortality")
mds.anaylsis = makeMDSAnalysis(mds.task)
mds.report = makeReport(mds.anaylsis)

test_that("makeMDSAnalysisReport", {
  expect_class(mds.report, "MDSAnalysisReport")
  expect_identical(length(mds.report), 4L)
  expect_list(mds.report$mds.analysis, len = 3L)
  expect_character(mds.report$report.id)
  expect_character(mds.report$type)
})

test_that("writeMDSAnalysisReport", {
  temp.wd = getwd()
  expect_file((rmd.file = writeReport(mds.report, save.mode = FALSE,
    override = TRUE)), extension = "rmd")
  expect_file(x = paste0("Data_Report/", mds.report$report.id, ".rds"))
  expect_identical(getwd(), temp.wd)
  rds.obj = readRDS(paste0("Data_Report/", mds.report$report.id, ".rds"))
  expect_equal(rds.obj$task, mds.report$task)
  expect_identical(rds.obj$report.id, rds.obj$report.id)
  expect_equal(class(rds.obj), class(mds.report))
  expect_error(writeReport(mds.report))
  expect_identical(getwd(), temp.wd)

  setwd(paste0(temp.wd, "/Data_Report"))
  knitr::knit2html(gsub("Data_Report/", "", rmd.file), quiet = TRUE)
  setwd(temp.wd)
  unlink("Data_Report", recursive = TRUE)
})
