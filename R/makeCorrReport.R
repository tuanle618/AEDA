#'
#'
#'
makeCorrReport = function(corr.obj, type){
  report.id = reportId()
  if (type == "CorrPlot"){
    plot.code = generateCorrPlot(corr.obj, report.id)
  }
  makeS3Obj2("CorrReport", corr.obj,
    plot.code = plot.code,
    report.id = report.id)
}

# my.task = makeCorrTask(id = "test", data = cars)
# my.corr = makeCorr(my.task)
# report1 = makeCorrReport(my.corr, type = "CorrPlot")
