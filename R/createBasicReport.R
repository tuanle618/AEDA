createBasicReport = function(id, data, target) {
  task = makeBasicReportTask(id = id, data = data, target = target)
  return(makeReport(task))
}
