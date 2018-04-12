context("finishReport")

test_that("finishReport Works", {

  data("Aids2", package = "MASS")
  set.seed(1)
  aids2 = Aids2[sample.int(500, 100), ]
  # Test with Aids subset dataset
  num.sum.task = makeNumSumTask(id = "aids",
    data = aids2, target = "T.categ")
  num.sum = makeNumSum(num.sum.task)
  num.sum.report = makeReport(num.sum)

  my.task = makeCorrTask(id = "aids",
    data = aids2, type = "CorrPlot")
  my.corr = makeCorr(my.task)
  corr.report = makeReport(my.corr)

  cat.sum.task = makeCatSumTask(id = "aids",
    data = aids2, target = "T.categ", na.rm = TRUE)
  cat.sum = makeCatSum(cat.sum.task)
  cat.sum.report = makeReport(cat.sum)

  my.report.task = makeBasicReportTask(id = "aids",
    data = aids2, target = "sex")
  basic.report = makeReport(my.report.task)

  my.cluster.task = makeClusterTask(id = "aids", data = aids2)
  cluster.analysis = makeClusterAnalysis(my.cluster.task)
  cluster.report = makeReport(cluster.analysis)

  #fake insert rownames as pX
  rownames(aids2) = paste0("p", rownames(aids2))
  my.mds.task = makeMDSTask(id = "aids", data = aids2)
  mds.analysis = makeMDSAnalysis(my.mds.task)
  mds.report = makeReport(mds.analysis)


  #combine all reports
  finishReport(basic.report, num.sum.report, corr.report,
   cat.sum.report, cluster.report, mds.report, save.mode = FALSE,
    override = TRUE)
  rmarkdown::render("MainReport.rmd", quiet = TRUE)

  expect_error(finishReport(basic.report, num.sum.report,
    corr.report, cat.sum.report, cluster.report, mds.report))

})
