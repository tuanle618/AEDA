createMDSReport = function(id, data, ...) {
  my.mds.task = makeMDSTask(id = id, data = data)
  mds.analysis = makeMDSAnalysis(my.mds.task)
  return(makeReport(mds.analysis))
}
