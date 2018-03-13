#' @title Creates a ClusterTask Object
#'
#' @description
#' A Task encapsulates the Data with some additional information
#'
#' @param id [\code{character(1)}]\cr
#'   ID of the Task Object
#' @param data [\code{data.frame}]\cr
#'   A Dataframe with different variables
#' @param method [\code{character(1)}]\cr
#'   Defines the clustering method
#'   Possible choices are: \cr
#'   For Hierarchical Clustering:
#'   \itemize{
#'   \item{\code{cluster.h}} - for more information @seealso \link[stats]{hclust}
#'   \item{\code{cluster.agnes}} - for more information @seealso \link[cluster]{agnes}
#'   \item{\code{cluster.diana}} - for more information @seealso \link[cluster]{diana}
#'   }
#'   For Partitioning Clustering:
#'   \itemize{
#'   \item{\code{cluster.kkmeans}} - for more information @seealso \link[kernlab]{kkmeans}
#'   \item{\code{cluster.kmeans}} - for more information @seealso \link[stats]{kmeans}
#'   \item{\code{cluster.pam}} - for more information @seealso \link[cluster]{pam}
#'   }
#'   For Model-Based Clustering:
#'   \itemize{
#'   \item{\code{cluster.dbscan}} - for more information @seealso \link[dbscan]{dbscan}
#'   \item{\code{cluster.mod}} - for more information @seealso \link[mclust]{mclust}
#'   }
#'   Default is \code{method = "cluster.kmeans"}
#' @param par.vals [\code{list}]\cr
#'   Additional arguments handled over to cluster algorithm \code{method}
#' @return ClusterTask Object
#' @examples
#' my.cluster.task = makeClusterTask(id = "iris", data = iris, target = "Species", method = "cluster.kmeans")
#' @import checkmate
#' @import BBmisc
#' @import cluster
#' @import kernlab
#' @import stats
#' @import mclust
#' @export
#'
makeClusterTask = function(id, data, target, method = "cluster.kmeans", par.vals){
  #Argument Checks
  assertCharacter(id, min.chars = 1L)
  assertDataFrame(data, col.names = "strict")
  #target will be checked within GetDataType
  assertChoice(method, choices = paste0("cluster.",
    c("h", "agnes", "diana", "kkmeans", "kmeans", "pam", "dbscan", "mod")))
  # Encapsulate Data and Data Types into new env
  env = new.env(parent = emptyenv())
  env$data = data
  env$datatypes = getDataType(data, target)

  makeS3Obj("ClusterTask",
    id = id,
    type = "ClusterSummary",
    env = env,
    size = nrow(data),
    numdatatypes = list(numeric = env$datatypes$num, integer = env$datatypes$int),
    method = method,
    par.vals = NULL
  )
}

#' @export
# Print fuction for NumTask Object
print.ClusterTask = function(x, ...) {
  catf("Task: %s", x$id)
  catf("Observations: %i", x$size)
  catf("Amount Numeric Columns: %i", length(x$numdatatypes$numeric))
  catf("Amount Integer Columns: %i", length(x$numdatatypes$integer))
  catf("Selected Method: %s", x$method)
}
