#' Creates an Categorical Summary Report object
#'
#' @param cat.sum.obj [\code{CatSumObj} Object]\cr
#'   A object of the "CatSumObj" class
#' @param type [\code{character(1)} Object]\cr
#'   The type of the Report to create. AS of now the default is \code{NULL}
#' @return CatSumReport
#' @import checkmate
#' @import BBmisc
#' @examples
#'  data("Arthritis", package = "vcd")
#'  cat.sum.task = makeCatSumTask(id = "Arthritis.Task", data = Arthritis, target = "Improved")
#'  cat.sum = makeCatSum(cat.sum.task)
#'  #create the numeric summary report
#'  cat.sum.report = makeCatSumReport(cat.sum)
#' @export
makeCatSumReport = function(cat.sum.obj, type = NULL){
  assertClass(cat.sum.obj, "CatSumObj")
  #assertCharacter(type, min.chars = 1)

  report.id = reportId()

  makeS3Obj("CatSumReport",
    cat.sum.task = cat.sum.obj$task,
    cat.sum = cat.sum$cat.sum,
    report.id = report.id,
    type = "CategoricalReport")
}

print.CatSumReport = function(x, ...) {
  print(x$cat.sum.task)
}
