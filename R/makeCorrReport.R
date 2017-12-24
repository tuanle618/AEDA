#'
#'
#'
makeCorrReport = function(corr.obj, type){
  var.id = collapse(sample(c(letters, LETTERS), size = 16, replace = TRUE), sep = "")
  if(type == "CorrPlot"){
    plot.code = generateCorrPlot(corr.obj, var.id)
  }
  makeS3Obj("CorrReport",
    plot.code = plot.code,
    var.id = var.id,
    corr.obj = corr.obj)
}

# my.task = makeCorrTask(id = "test", data = cars)
# my.corr = makeCorr(my.task)
# report1 = makeCorrReport(my.corr, type = "CorrPlot")
