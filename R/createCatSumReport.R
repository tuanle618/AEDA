createCatSumReport = function(id, data, target, ...) {
  cat.sum.task = makeCatSumTask(id = id, data = data, target = target, ...)
  cat.sum = makeCatSum(cat.sum.task)
  return(makeCatSumReport(cat.sum))
}
