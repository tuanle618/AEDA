createNumSumReport = function(id, data, target) {
  num.sum.task = makeNumSumTask(id = id, data = data, target = target)
  num.sum = makeNumSum(num.sum.task)
  return(makeNumSumReport(num.sum))
}
