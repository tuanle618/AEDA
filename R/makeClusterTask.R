#' @title Creates a ClusterTask Object
#'
#' @description
#' A Task encapsulates the Data with some additional information.\cr
#' As of now clustering will be only made for numerical data. Categorical data is [WIP]
#'
#' @param id [\code{character(1)}]\cr
#'   ID of the Task Object
#' @param data [\code{data.frame}]\cr
#'   A Dataframe with different variables
#' @param target [\code{character(1)}]\cr
#'   Target column of the dataset
#' @param cluster.cols [\code{character()}]\cr
#'   Named character vector to specify clusters. This only holds for\cr
#'   not hierarchical cluster methods. Default \code{cluster.cols = NULL}\cr .
#'   In default mode only datasets with maximal 10 numeric columns, a\cr
#'   cluster analysis will be with the combinations: choose(5,2).\cr
#'   If the amount of numeric columns is above 10, only the cluster for the PCA\cr
#'   for the numeric columns will be calculated and out ouf (k,2) combinations\cr
#'   randomly selected 10 combinations of tuples
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
#'   \item{\code{cluster.kmeans}} - for more information @seealso \link[stats]{kmeans}
#'   \item{\code{cluster.kkmeans}} - for more information @seealso \link[kernlab]{kkmeans}
#'   \item{\code{cluster.pam}} - for more information @seealso \link[cluster]{pam}
#'   }
#'   For Model-Based Clustering:
#'   \itemize{
#'   \item{\code{cluster.dbscan}} - for more information @seealso \link[dbscan]{dbscan}
#'   \item{\code{cluster.mod}} - for more information @seealso \link[mclust]{mclust}
#'   }
#'   Default is \code{method = "cluster.kmeans"}
#' @param random.seed [\code{integer(1)}]\cr
#'   Default is \code{random.seed = 89L}
#' @param scale.num.data [\code{logical(1L)}]\cr
#'   Logical whether to scale numeric data or not.\cr
#'   Default is \code{scale= TRUE}
#' @param par.vals [\code{list}]\cr
#'   Additional arguments handled over to cluster algorithm \code{method}.\cr
#'   Default is empty list \code{par.vals = list()}
#' @param show.NA.msg [\code{logical(1)}]\cr
#'  Logical whether to show missing values message\cr
#'  Default is \code{FALSE}.
#' @return ClusterTask Object
#' @examples
#' my.cluster.task = makeClusterTask(id = "iris", data = iris,
#'  target = "Species", method = "cluster.kmeans",
#'  random.seed = 89L, par.vals = list(iter.max = 15L))
#' my.cluster.task2 = makeClusterTask(id = "iris", data = iris,
#'  target = "Species", method = "cluster.kmeans",
#'  random.seed = 89L, cluster.cols = c("Sepal.Length" = "Petal.Length",
#'  "Sepal.Width" = "Petal.Width"))
#' @import checkmate
#' @import BBmisc
#' @importFrom cluster pam
#' @importFrom cluster diana
#' @importFrom cluster agnes
#' @importFrom kernlab kkmeans
#' @importFrom stats kmeans
#' @importFrom stats hclust
#' @importFrom mclust Mclust
#' @importFrom NbClust NbClust
#' @importFrom factoextra fviz_nbclust
#' @importFrom factoextra fviz_cluster
#' @importFrom factoextra get_dist
#' @importFrom factoextra fviz_dist
#' @importFrom factoextra eclust
#' @importFrom factoextra fviz_dend
#' @importFrom factoextra fviz_silhouette
#' @importFrom stats prcomp
#' @importFrom factoextra fviz_mclust
#' @importFrom dbscan dbscan
#' @import factoextra
#' @export
#'
makeClusterTask = function(id, data, target, cluster.cols = NULL, method = "cluster.kmeans", random.seed = 89L,
  scale.num.data = TRUE, par.vals = list(), show.NA.msg = FALSE){
  #check if numeric cols >= 2
  data.types = getDataType(data, target)
  if (length(c(data.types$num, data.types$int)) < 2) {
    stop(paste("Your dataset only contains of",
    length(c(data.types$num, data.types$int))), " numeric columns. Cluster Analysis does not make sense")
  }
  #Argument Checks
  assertCharacter(id, min.chars = 1L)
  assertDataFrame(data, col.names = "strict")
  #target will be checked within GetDataType

  #add warning for NAs:
  if (any(is.na(data)) & show.NA.msg) {
    message("The data set contains NAs.
These values might removed in the further calculations.
If so, another warning will be displayed.")
  }
  #check cluster.cols
  if (!is.null(cluster.cols)) {
    assertCharacter(cluster.cols, min.len = 1, max.len = 10)
    for (i in seq.int(length(cluster.cols))) {
      assertChoice(cluster.cols[i], choices = colnames(data))
      assertChoice(names(cluster.cols)[i], choices = colnames(data))
    }
  }
  assertChoice(method, choices = paste0("cluster.",
    c("h", "agnes", "diana", "kkmeans", "kmeans", "pam", "dbscan", "mod")))
  ##par.vals check::##
  ##check if names are in formals::     ###extra ... args for kkmeans, dbscan::dbscan and Mclust. --- let just function call and then error if false
  if (method == "cluster.h" | method == "cluster.agnes" | method == "cluster.kmeans" | method == "cluster.pam") {
    if (method == "cluster.h") {
      formals = formals(hclust)
    } else if (method == "cluster.agnes") {
      formals = formals(agnes)
    } else if (method == "cluster.diana") {
      formals = formals(diana)
    } else if (method == "cluster.kmeans") {
      formals = formals(kmeans)
    } else if (method == "cluster.pam") {
      formals = formals(pam)
    }
    for (arg in names(par.vals)) {
      if (!is.element(el = arg, set = names(formals))) {
        stop(paste(arg, "is not a parameter argument for clustering method:", method))
      }
    }
  }


  ####################
  # Encapsulate Data and Data Types into new env
  env = new.env(parent = emptyenv())
  env$data = data
  env$datatypes = getDataType(data, target)

  ##add option for Eps in dbscan and args in kkmeans
  if (length(par.vals) == 0) {
    if (method == "cluster.dbscan") {
      num.features = c(env$datatypes$num, env$datatypes$int)
      par.vals = list(eps = getEps(data[, num.features]))
    } else if (method == "cluster.kkmeans") {
      par.vals = list(centers = 2L)
    }
  } else if (length(par.vals) >= 1) {
    if (method == "cluster.dbscan" & !is.element(names(par.vals), "eps")) {
      num.features = c(env$datatypes$num, env$datatypes$int)
      par.vals = append(par.vals, list(eps = getEps(data[, num.features])))
    } else if (method == "cluster.kkmeans" & !is.element(names(par.vals), "centers")) {
      par.vals = append(par.vals, list(centers = 2L))
    }
  }

  makeS3Obj("ClusterTask",
    id = id,
    type = "ClusterSummary",
    env = env,
    size = nrow(data),
    numdatatypes = list(numeric = env$datatypes$num, integer = env$datatypes$int),
    method = method,
    par.vals = par.vals,
    random.seed = random.seed,
    scale.num.data = scale.num.data,
    cluster.cols = cluster.cols
  )
}

#' @export
# Print fuction for ClusterTask Object
print.ClusterTask = function(x, ...) {
  catf("Task: %s", x$id)
  catf("Observations: %i", x$size)
  catf("Amount Numeric Columns: %i", length(x$numdatatypes$numeric))
  catf("Amount Integer Columns: %i", length(x$numdatatypes$integer))
  catf("Selected Method: %s", x$method)
  catf("Selected Random Seed: %i", x$random.seed)
  if (length(x$par.vals) != 0) {
    catf("Additional params for method: %i", length(x$par.vals))
    print(unlist(x$par.vals))
  }
  catf("Scale numeric data: %s", x$scale.num.data)
}
