#' @title Creates a Multi Dimensional Scaling Task Object
#'
#' @description
#' A Task encapsulates the Data with some additional information.\cr
#' As of now MDS performs classical (weighted) MDS and non-metric methods
#'
#' @param id [\code{character(1)}]\cr
#'   ID of the Task Object
#' @param data [\code{data.frame}]\cr
#'   A Dataframe with different variables
#' @param target [\code{character(1)}]\cr
#'   Target column of the dataset
#' @param dist.norm [\code{character(1)}]\cr
#'   Character indicating how the distance matrix will be computed.\cr
#'   Possible values are:"euclidean", "maximum", "manhattan", \cr
#'   "canberra", "binary" or "minkowski".
#' @param method [\code{character(1)}]\cr
#'   Defines the MDS method
#'   Possible choices are: \cr
#'   For Metric Scaling:
#'   \itemize{
#'   \item{\code{cmdscale}} - for more information @seealso \link[stats]{cmdscale}
#'   \item{\code{wcmdscale}} - for more information @seealso \link[vegan]{wcmdscale}
#'   \item{\code{smacofSym}} - for more information @seealso \link[smacof]{smacofSym}
#'   }
#'   For Non-Metric Scaling:
#'   \itemize{
#'   \item{\code{isoMDS}} - for more information @seealso \link[MASS]{isoMDS}
#'   \item{\code{sammon}} - for more information @seealso \link[MASS]{sammon}
#'   }
#'   Default is \code{method = "cmdscale"}
#' @param par.vals [\code{list}]\cr
#'   Additional arguments handled over to MDS algorithm \code{method}.\cr
#'   Default is empty list \code{par.vals = list()}
#' @return MDSTask Object
#' @examples
#' data(swiss)
#' my.mds.task = makeMDSTask(id = "swiss", data = swiss,
#'  target = "Infant.Mortality", dist.norm = "euclidean",
#'  method = "cmdscale")
#' @import checkmate
#' @import BBmisc
#' @importFrom stats cmdscale
#' @export
#'
makeMDSTask = function(id, data, target, dist.norm = "euclidean", method = "cmdscale",
  par.vals = list()){

  data.types = getDataType(data, target)
  if (length(c(data.types$num, data.types$int)) < 2) {
    stop(paste("Your dataset only contains of",
      length(c(data.types$num, data.types$int))), " numeric columns. Multidimensional Scaling does not make sense")
  }
  #Argument Checks
  assertCharacter(id, min.chars = 1L)
  assertDataFrame(data, col.names = "strict")
  #Check rownames
  check = any((rownames(data) == as.character(seq_len(nrow(data)))))
  if (check) {
    warning(paste0("The dataset ", deparse(substitute(data)), " does not contain specific rownames."))
  }

  #target will be checked within GetDataType
  assertChoice(dist.norm, choices = c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski"))
  assertChoice(method, choices = c("cmdscale", "wcmdscale", "smacofSym", "isoMDS", "sammon"))
  ##par.vals check:
  formals = formals(method)
  for (arg in names(par.vals)) {
    if (!is.element(el = arg, set = names(formals))) {
        stop(paste(arg, "is not a parameter argument for MDS method:", method))
      }
  }

  #add k=2 for wcmdscale. Otherwise takes as default n-1 as dimension
  if (length(par.vals) == 0 & method == "wcmdscale") {
    par.vals = list(k = 2)
  } else if ((length(par.vals) > 0 & method == "wcmdscale") & !is.element("k", names(par.vals))) {
    par.vals = append(list(k = 2), par.vals)
  }

  #calculate distance:
  dist = dist(data, method = dist.norm)
  ####################
  # Encapsulate Data and Data Types into new env
  env = new.env(parent = emptyenv())
  env$data = data
  env$datatypes = getDataType(data, target)
  env$dist = dist

  makeS3Obj("MDSTask",
    id = id,
    type = "MDSSummary",
    env = env,
    size = nrow(data),
    numdatatypes = list(numeric = env$datatypes$num, integer = env$datatypes$int),
    method = method,
    par.vals = par.vals,
    dist.norm = dist.norm,
    dist = dist,
    target = target
  )
}

#' @export
# Print function for MDSTask Object
print.MDSTask = function(x, ...) {
  catf("Task: %s", x$id)
  catf("Observations: %i", x$size)
  catf("Amount Numeric Columns: %i", length(x$numdatatypes$numeric))
  catf("Amount Integer Columns: %i", length(x$numdatatypes$integer))
  catf("Selected Method: %s", x$method)
  if (length(x$par.vals) != 0) {
    catf("Additional params for method: %i", length(x$par.vals))
    print(unlist(x$par.vals))
  }
}
