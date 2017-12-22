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
#' @export
#' @import checkmate
#' @import BBmisc
#' @import moments
#' @export
makeNumSum = function(num.sum.task){
  assertClass(num.sum.task, "NumTask") #ToDO: change to NumSumTask (class) for more clarification (hence change makeNumSumTask.R)

  data = num.sum.task$env$data
  features = unlist(num.sum.task$numdatatypes)
  num.sum = "getNumSum(num.sum.task$env$data, unlist(numSum.task$numdatatypes))" #toDo : create this function

  makeS3Obj("NumSumObj",
    num.sum = num.sum,
    task = num.sum.task)
}
