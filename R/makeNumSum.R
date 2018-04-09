#' @title Calculates a Numeric Summary
#'
#' @description
#' makeNumSum calculates a numeric summary and wrapes an object around it
#'
#' @param num.sum.task [\code{NumSumTask}]\cr
#'   A NumSumTask Object
#' @return NumSumObj
#' @examples
#'  data("Aids2", package = "MASS")
#'  num.sum.task = makeNumSumTask(id = "AidsTask", data = Aids2,
#'    target = "sex")
#'  #get the numeric summary task object
#'  num.sum.result = makeNumSum(num.sum.task)
#' @import checkmate
#' @import BBmisc
#' @import moments
#' @import stats
#' @export
makeNumSum = function(num.sum.task){
  assertClass(num.sum.task, "NumSumTask")

  data = num.sum.task$env$data
  features = unlist(num.sum.task$numdatatypes)
  target = num.sum.task$env$datatypes$target
  geom.hist.args = num.sum.task$geom.hist.args
  geom.dens.args = num.sum.task$geom.dens.args
  geom.box.args = num.sum.task$geom.box.args
  num.sum = getNumSum(data, features, target, geom.hist.args, geom.dens.args, geom.box.args)

  makeS3Obj("NumSumObj",
    num.sum = num.sum$merged.list,
    num.sum.df = num.sum$num.sum.df,
    task = num.sum.task)
}

#' @export
# Print function for NumSum Object
print.NumSumObj = function(x, ...) {
  catf("Result of numeric/integer summary for: %s", x$task$id)
  cat("\n")
  print(x$num.sum.df)
}
