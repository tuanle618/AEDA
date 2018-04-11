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
#'   available reports if applicable to the data types provided by
#'   the dataset.
#' @param m.par.vals [\code{list()}]\cr
#'   A names list containing parameters which will be passed to
#'   the corresponding report function. [WIP]
#'
#' @examples
#' # Report for the iris dataset
#' \dontrun{
#' data("iris")
#' fastReport(data = iris, target = "Species")
#' }
#' @import checkmate
#' @importFrom OpenML getOMLDataSet
#' @export
fastReport = function(data, target = NULL, reports = c("Basic", "CatSum",
  "NumSum", "Corr", "Cluster", "PCA", "MDS", "FA"), m.par.vals = list()) {
  # argument checking
  assertDataFrame(data, col.names = "strict")
  assertCharacter(target, len = 1L, null.ok = TRUE)
  assertSubset(reports, c("Basic", "CatSum", "Corr", "NumSum", "MDS",
    "Cluster", "FA", "PCA"), empty.ok = FALSE)
  assertList(m.par.vals, names = "unique")
  assertSubset(names(m.par.vals), c("Basic", "CatSum", "Corr",
    "NumSum", "MDS", "Cluster", "FA", "PCA"))

  ##default is to call all reports.
  #include more stability with calling reports only if variable types are there:
  data.types = getDataType(data = data, target = target)
  numeric.dt = unique(c(data.types$int, data.types$num))
  categorical.dt = unique(c(data.types$ord, data.types$fact))
  #numeric report only if there is at least 1 numeric column
  if (length(numeric.dt) == 0) {
    #if 0 numeric columns, exclude numsum, corr, cluster, mds, fa, pca report
    reports = setdiff(reports, c("Corr", "NumSum", "MDS", "Cluster", "FA", "PCA"))
  } else if (length(numeric.dt) == 1) {
    # if contains 1 numeric column exclude corr, cluster, mds, fa, pca report
    reports = setdiff(reports, c("Corr", "MDS", "Cluster", "FA", "PCA"))
  } else if (length(numeric.dt) == 2) {
    # if contains 2 numeric column exclude cluster report
    reports = setdiff(reports, "PCA")
  } #else do no nothing and include all "numeric-reports", so keep default

  #categorical report only if there is at least 1 categorical column
  if (length(categorical.dt) == 0) {
    #if 0 categorical, exclude catsum
    reports = setdiff(reports, "CatSum")
  } #else do no nothing and include categorical, so keep after processing numeric checks


  # call create functions and save reports
  funs = paste0("create", reports, "Report")
  report.l = list()
  for (i in seq.int(funs)) {
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

