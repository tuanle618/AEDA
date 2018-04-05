#' Creates a full report for OpenML data sets
#'
#' @description
#' Imports a dataset from the openML data base and
#' creates a rmd report file
#'
#' @param data [\code{data.frame}]\cr
#'   A Dataframe with different variables
#' @param target [\code{character(1)}]\cr
#'  Target column. If not available please insert as \code{NULL}.
#' @param reports [\code{character()}]\cr
#'   The report types used on the dataset. Default will use all
#'   available reports
#' @param m.par.vals [\code{list()}]\cr
#'   A names list containing parameters which will be passed to
#'   the corressponding report function. [WIP]
#'
#' @examples
#' # Report for the iris dataset
#' \dontrun{
#' openMLReport(61L)
#' openMLReport(61L, "Cluster",
#'   m.par.vals = list(Cluster = list(method = "cluster.kmeans",
#'     par.vals = list(algorithm = "MacQueen"))))
#' }
#' @import checkmate
#' @importFrom OpenML getOMLDataSet
#' @export
fastReport = function(data, target = NULL, reports = c("Basic", "CatSum",
  "Corr", "NumSum", "MDS", "Cluster"), m.par.vals = list()) {
  # argument checking
  assertDataFrame(data, col.names = "strict")
  assertCharacter(target, len = 1L, null.ok = TRUE)
  assertSubset(reports, c("Basic", "CatSum", "Corr", "NumSum", "MDS",
    "Cluster"), empty.ok = FALSE)
  assertList(m.par.vals, names = "unique")
  assertSubset(names(m.par.vals), c("Basic", "CatSum", "Corr",
    "NumSum", "MDS", "Cluster"))

  # call create functions and save reports
  funs = paste0("create", reports, "Report")
  report.l = list()
  for (i in seq.int(funs)){
    string = funs[i]
    report = reports[i]
    message(string, "...")
    dots.arg = m.par.vals[[report]]

    args = append(list(data = data, id = "OpenMLReport",
      target = target), dots.arg)
    report.l[[string]] = do.call(string, args = args)
  }
  # finish the report
  args = append(report.l, c(save.mode = FALSE, override = TRUE))
  message("Write Report rmd ...")
  do.call(finishReport, args = args)
}

