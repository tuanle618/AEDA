#' @title Creates a full report for OpenML data sets
#' @param data.id [\code{integer()}]\cr
#'
#'
#'
#' @import checkmate
#' @importFrom OpenML getOMLDataSet
#' @export
openMLReport = function(data.id, reports = c("Basic", "CatSum",
  "Corr", "NumSum", "MDS"), m.par.vals = list()) {
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
  args = append(report.l, c(save.mode = FALSE, override = TRUE))
  do.call(finishReport, args = args)
}


#' @examples
#' openMLReport(61L)
#'
