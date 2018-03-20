#' @title Calculates the Correlation Matrix
#'
#' @description
#' makeCorr claculates the corr matrix with the predefines methode and wrapes an object around it
#'
#' @param corr.task [\code{CorrTask}]\cr
#'   A corrTask Object
#'
#'
#' @return CorrObject
#'
#' @examples
#' my.task = makeCorrTask(id = "test", data = cars)
#' my.corr = makeCorr(my.task)
#' my.corr$corr.matrix
#' @import checkmate
#' @import BBmisc
#' @importFrom stats cor
#' @export
makeCorr = function(corr.task){
  assertClass(corr.task, "CorrTask")

  data = corr.task$env$data
  features = unlist(corr.task$features)
  corr.matrix = cor(x = data[, features], method = corr.task$method)
  corr.task$needed.pkgs = c(corr.task$needed.pkgs, "stats")

  makeS3Obj2("CorrObj", corr.task,
    corr.matrix = corr.matrix)
}

#' @export
# Print fuction for CorrObj Object
print.CorrObj = function(x, ...) {
  catf("Task: %s", x$id)
  catf("Type: %2s", x$type)
  catf("Name of the Data: %s", x$data.name)
  cat("Correlationmatrix: \n")
  print(x$corr.matrix)
}
