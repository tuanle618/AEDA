context("Cluster Analysis")
data("iris")
set.seed(1L)
sub = sample.int(150, 12)
iris = iris[sub, 1:3]
data("InsectSprays")
my.cluster.task = makeClusterTask(id = "test.report", data = iris,
  method = "cluster.kmeans",
  random.seed = 1L, par.vals = list(iter.max = 15L))
test_that("makeClusterTask", {
  ##error for too less numeric columns
  expect_error({makeClusterTask(id = "insect.fail", data = InsectSprays)})

  expectIdentical(my.cluster.task$id, "test.report")
  expectIdentical(my.cluster.task$type, "ClusterSummary")
  expectIdentical(my.cluster.task$env$data, iris)
  expectIdentical(my.cluster.task$size, nrow(iris))
  expectIdentical(my.cluster.task$numdatatypes$numeric, c("Sepal.Length", "Sepal.Width", "Petal.Length"))
  expectIdentical(my.cluster.task$numdatatypes$integer, character(0))
  expectIdentical(my.cluster.task$method, "cluster.kmeans")
  expect_class(my.cluster.task, "ClusterTask")
  expectIdentical(my.cluster.task$par.vals, list(iter.max = 15L))
  expectIdentical(my.cluster.task$random.seed, 1L)
  expectIdentical(my.cluster.task$scale.num.data, TRUE)
  expect_null(my.cluster.task$cluster.cols)

  my.cluster.task.2 = makeClusterTask(id = "test.report", data = iris,
    method = "cluster.kmeans",
    random.seed = 1L, par.vals = list(iter.max = 15L),
    cluster.cols = c("Sepal.Width" = "Sepal.Length"))
  expectIdentical(my.cluster.task.2$cluster.cols, c("Sepal.Width" = "Sepal.Length"))
})

cluster.summary = makeClusterAnalysis(my.cluster.task)
test_that("makeClusterAnalysis", {
  expect_class(cluster.summary, "ClusterAnalysisObj")
  expectIdentical(cluster.summary$task, my.cluster.task)
  expectIdentical(length(cluster.summary), 2L)
  expectIdentical(length(cluster.summary$cluster.analysis), 2L)
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
  expectIdentical(cluster.report$task, my.cluster.task)
  expectIdentical(length(cluster.report), 4L)
  expectIdentical(cluster.report$type, "ClusterAnalysisReport")
  expect_character(cluster.report$report.id)
})

test_that("writeClusterAnalysisReport", {
  temp.wd = getwd()
  expect_file((rmd.file = writeReport(cluster.report, save.mode = FALSE, override = TRUE)), extension = "rmd")
  expect_file(x = paste0("Data_Report/", cluster.report$report.id, ".rds"))
  expectIdentical(getwd(), temp.wd)
  rds.obj = readRDS(paste0("Data_Report/", cluster.report$report.id, ".rds"))
  expectIdentical(rds.obj$cluster.analysis$cluster.all$cluster.res,
    cluster.report$cluster.analysis$cluster.all$cluster.res)
  expectIdentical(rds.obj$cluster.analysis$comb.cluster.list[[1]]$cluster.res,
    cluster.report$cluster.analysis$comb.cluster.list[[1]]$cluster.res)
  expectIdentical(rds.obj$cluster.analysis$comb.cluster.list[[2]]$cluster.res,
    cluster.report$cluster.analysis$comb.cluster.list[[2]]$cluster.res)
  expectIdentical(rds.obj$cluster.analysis$comb.cluster.list[[3]]$cluster.res,
    cluster.report$cluster.analysis$comb.cluster.list[[3]]$cluster.res)

  expect_error(writeReport(cluster.report))
  expectIdentical(getwd(), temp.wd)

  setwd(paste0(temp.wd, "/Data_Report"))
  knitr::knit2html(gsub("Data_Report/", "", rmd.file), quiet = TRUE)
  setwd(temp.wd)
  unlink("Data_Report", recursive = TRUE)
})
