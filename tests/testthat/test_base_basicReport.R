context("Basic Data Summary")
data("airquality")
test_that("makeReportTask", {
  basic.report.task = makeBasicReportTask(id = "test.report", data = airquality, target = "Wind")
  expectIdentical(basic.report.task$missing.values, sum(is.na(airquality)))
  expectIdentical(basic.report.task$size, nrow(airquality))
  expectIdentical(basic.report.task$env$data, airquality)
  expect_class(basic.report.task, "BasicReportTask")
  expectIdentical(basic.report.task$target, "Wind")
  expectIdentical(basic.report.task$id, "test.report")
  expectIdentical(basic.report.task$dataset.name, "airquality")
  basic.report.task = makeBasicReportTask(id = "test.report", data = airquality, target = NULL)
  expect_error(makeReportTask(id = "test.report", data = airquality))
  expect_error(makeReportTask(data = airquality, target = "Wind"))
})

test_that("makeBasicReport", {
  basic.report.task = makeBasicReportTask(id = "test.report", data = airquality, target = "Wind")
  basic.report = makeBasicReport(basic.report.task)
  expect_class(basic.report, "BasicReport")
  expectIdentical(basic.report$task, basic.report.task)
  expectIdentical(length(basic.report), 5L)
  expectIdentical(length(basic.report$basic.data.summary), 3L)
  expectIdentical(length(basic.report$na.summary), 6L)
  expect_character(basic.report$report.id)
  expectIdentical(basic.report$type, "BasicReport")

  basic.report.task = makeBasicReportTask(id = "test.report", data = airquality, target = NULL)
  basic.report = makeBasicReport(basic.report.task)
})

test_that("writeBasicReport", {
  basic.report.task = makeBasicReportTask(id = "test.report", data = airquality, target = "Wind")
  basic.report = makeBasicReport(basic.report.task)
  temp.wd = getwd()
  expect_file((rmd.file = writeReport(basic.report)), extension = "rmd")
  expect_file(x = paste0("Data_Report/", basic.report$report.id, ".rds"))
  expectIdentical(getwd(), temp.wd)
  rds.obj = readRDS(paste0("Data_Report/", basic.report$report.id, ".rds"))

  expect_equal(rds.obj$task, basic.report$task)

  expectIdentical(rds.obj$basic.data.summary, basic.report$basic.data.summary)
  expectIdentical(rds.obj$na.summary[!names(rds.obj$na.summary) %in% c("env", "image", "ggplot")],
    basic.report$na.summary[!names(basic.report$na.summary) %in% c("env", "image", "ggplot")])

  expect_error(writeReport(basic.report))
  expectIdentical(getwd(), temp.wd)
  writeReport(basic.report, save.mode = FALSE, override = TRUE)
  expectIdentical(getwd(), temp.wd)

  setwd(paste0(temp.wd, "/Data_Report"))
  knitr::knit2html(gsub("Data_Report/", "", rmd.file), quiet = TRUE)
  setwd(temp.wd)
  unlink("Data_Report", recursive = TRUE)
})
