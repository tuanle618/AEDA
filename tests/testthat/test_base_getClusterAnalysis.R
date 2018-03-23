context("getClusterAnalysis")
data("iris")
set.seed(1L)
sub = sample.int(150,20)
iris = iris[sub,]
test_that("getClusterAnalysis kmeans", {
  clustered = getClusterAnalysis(data = iris, num.features = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),
    random.seed = 1L, par.vals = list(iter.max = 15L), scale.num.data = TRUE, method = "cluster.kmeans")
  expect_identical(class(clustered$cluster.all$cluster.res), "kmeans")
  expect_list(clustered$cluster.all$cluster.diag, min.len = 1L)
  lapply(clustered$cluster.all$cluster.diag, FUN = function(x) {
    expect_identical(class(x), c("gg", "ggplot"))
  })
  expect_length(clustered$comb.cluster.list, 6L)
})

test_that("getClusterAnalysis pam", {
  clustered = getClusterAnalysis(data = iris, num.features = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),
    random.seed = 1L, par.vals = list(), scale.num.data = TRUE, method = "cluster.pam")
  expect_identical(class(clustered$cluster.all$cluster.res), c("pam", "partition"))
  expect_list(clustered$cluster.all$cluster.diag, min.len = 1L)
  lapply(clustered$cluster.all$cluster.diag, FUN = function(x) {
    expect_identical(class(x), c("gg", "ggplot"))
  })
  expect_length(clustered$comb.cluster.list, 6L)

  clustered.manhattan = getClusterAnalysis(data = iris, num.features = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),
    random.seed = 1L, par.vals = list(metric = "manhattan"), scale.num.data = TRUE, method = "cluster.pam")
  # check if clusters are different -> is par.vals working correctly
  expect_false(isTRUE(all.equal(clustered$cluster.all$cluster.res$clusinfo,
    clustered.manhattan$cluster.all$cluster.res$clusinfo)))
})

test_that("getClusterAnalysis cluster.h", {
  clustered = getClusterAnalysis(data = iris, num.features = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),
    random.seed = 1L, par.vals = list(), scale.num.data = TRUE, method = "cluster.h")
  expect_identical(class(clustered$cluster.all$cluster.res), c("hclust", "hcut", "eclust"))
  expect_list(clustered$cluster.all$cluster.diag, min.len = 1L)
  lapply(clustered$cluster.all$cluster.diag, FUN = function(x) {
    expect_identical(class(x), c("gg", "ggplot"))
  })
  expect_list(clustered$comb.cluster.list, len = 0L)

  clustered.manhattan = getClusterAnalysis(data = iris, num.features = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),
    random.seed = 1L, par.vals = list(hc_metric = "manhattan"), scale.num.data = TRUE, method = "cluster.h")
  # check if clusters are different -> is par.vals working correctly
  expect_false(clustered$cluster.all$cluster.res$dist.method == clustered.manhattan$cluster.all$cluster.res$dist.method)
})
