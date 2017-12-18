#' Get general characteristics of data.frame.
#'
#' @param data [\code{data.frame}]\cr
#'   Data to summarize.
#' @param target [\code{character(1)}]\cr
#'   Target column. If not available please insert as \code{NULL}.
#' @param large [\code{numeric(1)}]\cr
#'   What is a large (absolute) value for a numeric column?
#' @param feat.perc [\code{logical(1)}]\cr
#'   Portion of features in percent? Default is \code{FALSE}.
#' @param na.perc [\code{logical(1)}]\cr
#'   Portion of NAs in percent? Default is \code{FALSE}.
#' @param class.perc [\code{logical(1)}]\cr
#'   Portion of classes in percent? Default is \code{FALSE}.
#' @param large.perc [\code{logical(1)}]\cr
#'   Portion of large values in percent? Default is \code{FALSE}.
#' @examples
#'   data("iris")
#'   BasicDataSummary(iris, target = "Species")
#' @return [named \code{list}], containing characteristics.
#' @export

BasicDataSummary = function(data, target, large = 1e10,
  feat.perc = FALSE, na.perc = FALSE, class.perc = FALSE, large.perc = FALSE) {

  #Argument checking:
  assertNumeric(large, len = 1)
  assertLogical(feat.perc, len = 1)
  assertLogical(na.perc, len = 1)
  assertLogical(class.perc, len = 1)
  assertLogical(large.perc, len = 1)

  DataTypeList = getDataType(data, target)
  if (!is.null(target)) {
    tt = data[, target]
  } else tt = NULL


  #Defining output vector
  x = numeric(0)

  NAs = sum(is.na(data))
  rows.with.missings = sum(apply(data, 1, function(x) any(is.na(x))))
  cols.with.missings = sum(apply(data, 2, function(x) any(is.na(x))))

  x["obs"] = nrow(data)
  x["NAs"] = NAs
  x["dim"] = length(DataTypeList$X)

  x["num"] = length(DataTypeList$num) - ifelse(is.null(tt),1,0)
  x["int"] = length(DataTypeList$int) - is.integer(tt)
  x["fact"] = length(DataTypeList$fact) - is.factor(tt)
  x["char"] = length(DataTypeList$char) - is.character(tt)
  x["log"] = length(DataTypeList$logic) - is.logical(tt)
  x["Date"] = length(DataTypeList$date)
  if (feat.perc) {
    x["num"] = x["num"] / x["dim"]
    x["int"] = x["int"] / x["dim"]
    x["fact"] = x["fact"] / x["dim"]
    x["char"] = x["char"] / x["dim"]
    x["log"] = x["log"] / x["dim"]
    x["Date"] = x["Date"] / x["dim"]
  }

  x["na.row.max"] = max(Reduce(function(a,b) is.na(a) + is.na(b), data, init = 0))
  x["na.col.max"] = max(sapply(data, function(y) sum(is.na(y))))
  if (na.perc) {
    x["na.row.max"] = x["na.row.max"] / x["dim"]
    x["na.col.max"] = x["na.col.max"] / x["obs"]
  }

  g = function(y) if (is.numeric(y)) as.integer(y >= large) else 0
  x["large.row.max"] = max(Reduce(function(a,b) a + g(b), data, init = 0))
  x["large.col.max"] = max(sapply(data, function(y)
    if (is.numeric(y)) sum(abs(y) >= large) else 0))
  if (large.perc) {
    x["large.row.max"] = x["large.row.max"] / x["dim"]
    x["large.col.max"] = x["large.col.max"] / x["obs"]
  }

  if (is.factor(tt) || is.character(tt)) {
    tab = table(tt)
    tab2 = prop.table(tab)
    x["classes"] = sum(tab)
    if (class.perc) {
      x["class.max"] = as.numeric(min(tab2))
      x["class.min"] = as.numeric(max(tab2))
    } else {
      x["class.max"] = min(tab)
      x["class.min"] = max(tab)
    }
    x["class.quot"] = x["class.max"] / x["class.min"]
  }
  basicsummarylist = as.list(x)
  basicsummarylist$target = ifelse(!is.null(target), class(data[,target]),NULL)
  basicsummarylist$DataTypeList = DataTypeList
  class(basicsummarylist) = append(class(basicsummarylist), "reportBasicSummary")
  return(basicsummarylist)
}
