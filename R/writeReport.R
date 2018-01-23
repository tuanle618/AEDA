writeReport = function(x, ...) UseMethod("writeReport")

writeReport.default = function(x, ...){
  warning(paste0("writeReport does not know how to handle object of class \"",
    class(x), "\""))
}
