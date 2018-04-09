#' Creates an Factor Analysis Report Object
#'
#' @param fa.obj [\code{FAObj} Object]\cr
#'   A object of the "FAObj" class
#' @return A FAReport Object
#' @import checkmate
#' @import BBmisc
#' @examples
#'  library(psych)
#'  data(bfi)
#'  #take small sample of size 200L:
#'  bfi_small = bfi[sample(seq_len(nrow(bfi)), size = 200L), ]
#'  FA.task = makeFATask(id = "bfi", data = bfi_small)
#'  FA.result = makeFA(FA.task)
#'  FA.report = makeFAReport(FA.result)
#' @export
makeFAReport = function(fa.obj){
  assertClass(fa.obj, "FAObj")

  report.id = reportId()

  makeS3Obj("FAReport",
    task = fa.obj$task,
    fa.result = fa.obj$fa.result,
    report.id = report.id,
    type = "FAReport")
}

print.FAReport = function(x, ...) {
  print(x$task)
}
