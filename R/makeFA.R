#' @title Calculates a Factor Analysis
#'
#' @description
#' makeFA calculates a exploratory factor analysis and wrapes an object around it
#'
#' @param fa.task [\code{FATask}]\cr
#'   A FATask Object
#' @return FA Object
#' @examples
#'  library(psych)
#'  data(bfi)
#'  #take small sample of size 200L:
#'  bfi_small = bfi[sample(seq_len(nrow(bfi)), size = 200L), ]
#'  my.FA.task = makeFATask(id = "bfi", data = bfi_small)
#'  my.FA = makeFA(my.FA.task)
#' @import checkmate
#' @import BBmisc
#' @importFrom psych fa
#' @importFrom psych fa.diagram
#' @export
makeFA = function(fa.task){
  assertClass(fa.task, "FATask")

  num.data = fa.task$env$num.data
  num.features = unlist(fa.task$numdatatypes)
  rotate = fa.task$rotate
  par.vals = fa.task$par.vals
  nfactors = fa.task$nfactors
  fa.result = do.call(what = fa, args = append(list(r = num.data, nfactors = nfactors), par.vals))

  makeS3Obj("FAObj",
    task = fa.task,
    fa.result = fa.result
  )
}

#' @importFrom psych fa.diagram
#' @export
# Print function for FA Object
print.FAObj = function(x, ...) {
  print(x$task)
}
