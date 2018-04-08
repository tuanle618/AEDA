context("Fast and OpenML Report")
data("iris")
set.seed(1L)
sub = sample.int(150, 12)
iris = iris[sub, ]

test_that("fastReport", {
  start.wd = getwd()
  dir.create("test_folder")
  setwd("test_folder/")
  tmp.wd = getwd()
  suppressWarnings(fastReport(iris, reports = c("Basic", "CatSum", "Corr", "NumSum",
    "MDS", "FA", "PCA")))
  expectIdentical(getwd(), tmp.wd)
  expect_file("MainReport.rmd")
  expect_file("Data_Report/BasicReport1.rmd")
  expect_file("Data_Report/CatSumReport1.rmd")
  expect_file("Data_Report/CorrReport1.rmd")
  expect_file("Data_Report/MDSAnalysisReport1.rmd")
  expect_file("Data_Report/NumSumReport1.rmd")
  expect_file("Data_Report/PCAReport1.rmd")
  expect_file("Data_Report/FactorAnalysisReport1.rmd")
  rmarkdown::render("MainReport.rmd")

  ## Test m.par.vals for Cluster
  m.par.vals = list(
    Cluster = list(
      method = "cluster.kmeans",
      par.vals = list(algorithm = "MacQueen", iter.max = 1L)
    )
  )
  # indirect testing by producing an warining or error
  expect_warning(fastReport(iris, reports = "Cluster", m.par.vals = m.par.vals),
    "did not converge in 1 iteration")

  ## Test m.par.vals for CatSum
  m.par.vals = list(
    CatSum = list(position = "error")
  )
  # indirect testing by producing an warining or error
  expect_error(fastReport(iris, reports = "CatSum", m.par.vals = m.par.vals))

  ## Test m.par.vals for Corr
  m.par.vals = list(
    Corr = list(method = "error")
  )
  # indirect testing by producing an warining or error
  expect_error(fastReport(iris, reports = "Corr", m.par.vals = m.par.vals),
    "[Assertion on 'method' failed: Must be a subset of] \\{'pearson','spearman'\\}")

  ## Test m.par.vals for MDS
  m.par.vals = list(
    MDS = list(method = "error")
  )
  # indirect testing by producing an warining or error
  expect_error(fastReport(iris, reports = "MDS", m.par.vals = m.par.vals),
    "Assertion on 'method' failed")
  #because line gets too long: again
  expect_error(fastReport(iris, reports = "MDS", m.par.vals = m.par.vals),
    "'cmdscale','wcmdscale','smacofSym','isoMDS','sammon'")

  ## Test m.par.vals for NumSum
  m.par.vals = list(
    NumSum = list(geom.box.args = list(position = "error"))
  )
  # indirect testing by producing an warining or error
  expect_error(fastReport(iris, reports = "NumSum", m.par.vals = m.par.vals),
    "No position called 'error'.")

  ## Test m.par.vals for FA
  ##toDo
  # indirect testing by producing error

  ## Test m.par.vals for PCA
  m.par.vals = list(
    PCA = list(center.data = TRUE)
  )
  # indirect testing by producing an warining or error
  expect_warning(fastReport(iris, reports = "PCA", m.par.vals = m.par.vals))

  setwd(start.wd)
  unlink("test_folder", recursive = TRUE, force = TRUE)
})

test_that("openMLReport", {
  start.wd = getwd()
  dir.create("test_folder")
  setwd("test_folder/")
  tmp.wd = getwd()
  openMLReport(61L, reports = c("Basic", "CatSum", "Corr", "NumSum",
    "MDS", "FA", "PCA"))
  expectIdentical(getwd(), tmp.wd)
  expect_file("MainReport.rmd")
  expect_file("Data_Report/BasicReport1.rmd")
  expect_file("Data_Report/CatSumReport1.rmd")
  expect_file("Data_Report/CorrReport1.rmd")
  expect_file("Data_Report/MDSAnalysisReport1.rmd")
  expect_file("Data_Report/NumSumReport1.rmd")
  expect_file("Data_Report/FactorAnalysisReport1.rmd")
  expect_file("Data_Report/PCAReport1.rmd")
  rmarkdown::render("MainReport.rmd")
  setwd(start.wd)
  unlink("test_folder", recursive = TRUE, force = TRUE)
})
