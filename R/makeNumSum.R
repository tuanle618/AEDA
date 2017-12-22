#' @title Calculates a Numeric Summary
#'
#' @description
#' makeNumSum calculates a numeric summary and wrapes an object around it
#'
#' @param num.sum.task [\code{NumTask}]\cr
#'   A NumSumTask Object
#' @return NumSumObj
#' @examples
#'  data("Boston", package = "MASS")
#'  num.sum.task = makeNumSumTask(id = "BostonTask", data = Boston, target = "medv")
#'  #get the numeric summary task object
#'  num.sum = makeNumSum(num.sum.task)
#' @import checkmate
#' @import BBmisc
#' @import moments
#' @import stats
#' @export
makeNumSum = function(num.sum.task){
  assertClass(num.sum.task, "NumSumTask")

  data = num.sum.task$env$data
  features = unlist(num.sum.task$numdatatypes)
  num.sum = getNumSum(data, features, target)

  makeS3Obj("NumSumObj",
    num.sum = num.sum,
    task = num.sum.task)
}
