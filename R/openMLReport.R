#' Creates a full report for OpenML data sets
#'
#' @description
#' Imports a dataset from the openML data base and
#' creates a rmd report file
#'
#' @param data.id [\code{integer(1)}]\cr
#'   data.id is the id from openML datasets see
#'   \code{\link[OpenML]{getOMLDataSet}}
#' @param reports [\code{character()}]\cr
#'   The report types used on the dataset. Default will use all
#'   available reports if they are applicable
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
openMLReport = function(data.id, reports = c("Basic", "CatSum",
  "Corr", "NumSum", "Cluster", "MDS", "PCA", "FA"), m.par.vals = list()) {
  # argument checking
  assertInteger(data.id, lower = 0L, any.missing = FALSE,
    all.missing = FALSE, len = 1L, null.ok = FALSE)
  assertSubset(reports, c("Basic", "CatSum", "Corr", "NumSum", "MDS",
    "Cluster", "FA", "PCA"), empty.ok = FALSE)
  assertList(m.par.vals, names = "unique")
  assertSubset(names(m.par.vals), c("Basic", "CatSum", "Corr",
    "NumSum", "MDS", "Cluster", "FA", "PCA"))

  # load openML data
  data.set = getOMLDataSet(data.id)
  target = data.set$target.features
  data = data.set$data

  fastReport(data = data, target = target, reports = reports,
    m.par.vals = m.par.vals)
}

