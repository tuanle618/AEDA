#' Creates an Factor Analysis Report Object
#'
#' @param analysis.obj [\code{FAObj} Object]\cr
#'   A object of the "FAObj" class
#' @return A FAReport Object
#' @import checkmate
#' @import BBmisc
#' @examples
#'  library(psych)
#'  data(bfi)
#'  #take small sample of size 200L:
#'  bfi_small = bfi[sample(seq_len(nrow(bfi)), size = 200L), ]
#'  my.FA.task = makeFATask(id = "bfi", data = bfi_small)
#'  my.FA = makeFA(my.FA.task)
#'  my.FA.report = makeReport(my.FA)
#' @export
makeReport.FAObj = function(analysis.obj){
  assertClass(analysis.obj, "FAObj")

  report.id = reportId()

  makeS3Obj("FAReport",
    task = analysis.obj$task,
    fa.result = analysis.obj$fa.result,
    report.id = report.id,
    type = "FAReport")
}

print.FAReport = function(x, ...) {
  print(x$task)
}
