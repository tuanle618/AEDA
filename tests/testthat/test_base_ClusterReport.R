context("Cluster Analysis")
data("iris")
set.seed(1L)
sub = sample.int(150, 12)
iris = iris[sub, 1:3]
my.cluster.task = makeClusterTask(id = "test.report", data = iris,
  target = "Species", method = "cluster.kmeans",
  random.seed = 1L, par.vals = list(iter.max = 15L))
test_that("makeClusterTask", {
  expect_identical(my.cluster.task$id, "test.report")
  expect_identical(my.cluster.task$type, "ClusterSummary")
  expect_identical(my.cluster.task$env$data, iris)
  expect_identical(my.cluster.task$size, nrow(iris))
  expect_identical(my.cluster.task$numdatatypes$numeric, c("Sepal.Length", "Sepal.Width", "Petal.Length"))
  expect_identical(my.cluster.task$numdatatypes$integer, character(0))
  expect_identical(my.cluster.task$method, "cluster.kmeans")
  expect_class(my.cluster.task, "ClusterTask")
  expect_identical(my.cluster.task$par.vals, list(iter.max = 15L))
  expect_identical(my.cluster.task$random.seed, 1L)
  expect_identical(my.cluster.task$scale.num.data, TRUE)
  expect_null(my.cluster.task$cluster.cols)

  my.cluster.task.2 = makeClusterTask(id = "test.report", data = iris,
    target = "Species", method = "cluster.kmeans",
    random.seed = 1L, par.vals = list(iter.max = 15L),
    cluster.cols = c("Sepal.Width" = "Sepal.Length"))
  expect_identical(my.cluster.task.2$cluster.cols, c("Sepal.Width" = "Sepal.Length"))
})

cluster.summary = makeClusterAnalysis(my.cluster.task)
test_that("makeClusterAnalysis", {
  expect_class(cluster.summary, "ClusterAnalysisObj")
  expect_identical(cluster.summary$task, my.cluster.task)
  expect_identical(length(cluster.summary), 2L)
  expect_identical(length(cluster.summary$cluster.analysis), 2L)
  #check if plots have no errors
  cluster.summary$cluster.analysis$cluster.all$cluster.plot
  cluster.summary$cluster.analysis$cluster.all$cluster.diag
  cluster.summary$cluster.analysis$comb.cluster.list[[1]]
  cluster.summary$cluster.analysis$comb.cluster.list[[2]]
  cluster.summary$cluster.analysis$comb.cluster.list[[3]]
  expect_error(cluster.summary$cluster.analysis$comb.cluster.list[[4]])
})

cluster.report = makeReport(cluster.summary)
test_that("makeClusterAnalysisReport", {
  expect_class(cluster.report, "ClusterAnalysisReport")
  expect_identical(cluster.report$report.task, my.cluster.task)
  expect_identical(length(cluster.report), 4L)
  expect_identical(cluster.report$type, "ClusterAnalysisReport")
  expect_character(cluster.report$report.id)
})

test_that("writeClusterAnalysisReport", {
  temp.wd = getwd()
  expect_file((rmd.file = writeReport(cluster.report, save.mode = FALSE, override = TRUE)), extension = "rmd")
  expect_file(x = paste0("Data_Report/", cluster.report$report.id, ".rds"))
  expect_identical(getwd(), temp.wd)
  rds.obj = readRDS(paste0("Data_Report/", cluster.report$report.id, ".rds"))
  expect_identical(rds.obj$cluster.analysis$cluster.all$cluster.res,
    cluster.report$cluster.analysis$cluster.all$cluster.res)
  expect_identical(rds.obj$cluster.analysis$comb.cluster.list[[1]]$cluster.res,
    cluster.report$cluster.analysis$comb.cluster.list[[1]]$cluster.res)
  expect_identical(rds.obj$cluster.analysis$comb.cluster.list[[2]]$cluster.res,
    cluster.report$cluster.analysis$comb.cluster.list[[2]]$cluster.res)
  expect_identical(rds.obj$cluster.analysis$comb.cluster.list[[3]]$cluster.res,
    cluster.report$cluster.analysis$comb.cluster.list[[3]]$cluster.res)

  expect_error(writeReport(cluster.report))
  expect_identical(getwd(), temp.wd)

  setwd(paste0(temp.wd, "/Data_Report"))
  knitr::knit2html(gsub("Data_Report/", "", rmd.file), quiet = TRUE)
  setwd(temp.wd)
  unlink("Data_Report", recursive = TRUE)
})
