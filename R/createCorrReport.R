createCorrReport = function(id, data, ...) {
  my.task = makeCorrTask(id = "OpenMLReport", data = data)
  my.corr = makeCorr(my.task)
 return(makeCorrReport(my.corr))
}
