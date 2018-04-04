#' @title Creates a full report for OpenML data sets
#' @param data.id [\code{integer(1)}]\cr
#'   data.id is the id from openML datasets see
#'   \code{\link[openML]{getOMLDataSet}}
#' @param reports [\code{character()}]\cr
#'   The report types used on the dataset. Default will use all
#'   available reports
#' @param m.par.vals [\code{list()}]\cr
#'   A names list containing parameters which will be passed to
#'   the corressponding report function. [WIP]
#'
#' @examples
#' # Report for the iris dataset
#' openMLReport(61L)
#'
#' @import checkmate
#' @importFrom OpenML getOMLDataSet
#' @export
openMLReport = function(data.id, reports = c("Basic", "CatSum",
  "Corr", "NumSum", "MDS", "Cluster"), m.par.vals = list()) {
  # argument checking
  assertInteger(data.id, lower = 0L, any.missing = FALSE,
    all.missing = FALSE, len = 1L, null.ok = FALSE)
  assertSubset(reports, c("Basic", "CatSum", "Corr", "NumSum", "MDS",
    "Cluster"), empty.ok = FALSE)
  assertList(m.par.vals, names = "unique")
  assertSubset(names(m.par.vals), c("Basic", "CatSum", "Corr",
    "NumSum", "MDS", "Cluster"))

  # load openML data
  data.set = getOMLDataSet(data.id)
  target = data.set$target.features
  data = data.set$data

  # call create functions and save reports
  funs = paste0("create", reports, "Report")
  report.l = list()
  for(string in funs){
    message(string, "...")
    report.l[[string]] = do.call(string, args = list(data = data, id = "OpenMLReport",
      target = target))
  }
  # finish the report
  args = append(report.l, c(save.mode = FALSE, override = TRUE))
  message("Write Report rmd ...")
  do.call(finishReport, args = args)
}
