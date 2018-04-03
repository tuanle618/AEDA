#' @title Creates a full report for OpenML data sets
#'
#'
#'
#' @import checkmate
#' @importFrom OpenML getOMLDataSet
#' @export
openMLReport = function(data.id, reports = c("Basic", "CatSum",
  "Cluster", "Corr", "NumSum", "MDS"), m.par.vals = list()) {
  data.set = getOMLDataSet(data.id)
  target = data.set$target.features
  data = data.set$data

  funs = paste0("create", reports, "Report")
  report.l = list()
  for(string in funs){
    print(paste("Creating", string, "Report ..."))
    report.l[[string]] = do.call(string, args = list(data = data, id = "OpenMLReport",
      target = target))
  }

  finishReport()
}
