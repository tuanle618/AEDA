#' @title Creates a ReportTask Object
#'
#' @description
#' A Report Task encapsulates the Data with some additional information
#'
#' @param id [\code{character(1)}]\cr
#'   ID of the Task Object
#' @param data [\code{data.frame}]\cr
#'   A Dataframe with different variables
#' @param target [\code{character(1)}]\cr
#'   Defines the target column. f the dataset does not contain a target column please insert \code{NULL}
#'
#' @return [\code{BasicReportTask}]
#'
#' @examples
#' my.basic.report.task = makeBasicReportTask(id = "report.test", data = iris, target = "Species")
#' # Extract Data
#' my.basic.report.task$env$data
#' @import checkmate
#' @import BBmisc
#' @export
makeBasicReportTask = function(id, data, target){
  # Argument Checks
  assertCharacter(id, min.chars = 1L)
  assertDataFrame(data, col.names = "strict")
  if (!is.null(target))
    assertCharacter(target, min.chars = 1L, len = 1L)

  # Encapsulate Data into new env
  env = new.env(parent = emptyenv())
  env$data = data
  env$target = target

  makeS3Obj("BasicReportTask",
    id = id,
    target = target,
    dataset.name = deparse(substitute(data)),
    env = env,
    size = nrow(data),
    missing.values = sum(is.na(data)))
}

#' @export
# Print fuction for ReportTask Object
print.BasicReportTask = function(x, ...) {
  catf("Task: %s", x$id)
  catf("Dataset: %s", x$dataset.name)
  catf("Target: %s", x$target)
  catf("Features: %s", collapse(colnames(x$env$data), sep = ", "))
  catf("Observations: %i", x$size)
  catf("Missing Values: %s", x$missing.values)
}
