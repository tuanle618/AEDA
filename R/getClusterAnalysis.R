#' @title Computes the components and plots for the Cluster Analysis
#'
#' @description
#'  getClusterAnalysis computes the cluster analysis of numeric data
#'
#' @param data [\code{data.frame}]\cr
#'   A Dataframe with different variables.
#' @param num.features [\code{character(length(numeric.features))}]\cr
#'   A character vector with length of the number of numeric features in the dataset.
#'   This will be computed automatically when calling this function.
#' @param method [\code{character(1L)}]\cr
#'   Method used for clustering
#' @param par.vals [\code{list()}]\cr
#'   Other arguments to be passed to selected method
#' @param random.seed [\code{integer(1L)}]\cr
#'   Random seed for clustering method
#' @param scale [\code{logical(1L)}]\cr
#'   Logical whether or not so scale numeric data for cluster analysis
#' @return [\code{list()}]
#'   A list containing the cluster analysis and plot-code
#' @import stats
#' @import cluster
#' @import kernlab
#' @import mclust
#' @import mclust
#' @import NbClust
#' @import factoextra
#'
getClusterAnalysis = function(data, num.features, method, par.vals, random.seed, scale.num.data) {
  #select numeric data
  set.seed(random.seed)
  num.data = data[, num.features]
  if (scale.num.data) num.data = scale(num.data)
  #tupel combinations
  combinations = combn(x = 1:ncol(num.data), m = 2)
  if (method == "cluster.kmeans") {
    ###### all numeric colums together: ######
    ###Optimal amount of clusters:
    wss = (nrow(num.data) - 1)*sum(apply(num.data, 2, var))
    for (i in 2:15) wss[i] = sum(kmeans(num.data,
      centers = i, iter.max = 20L)$withinss)
    ##Elbowplot
    plot(1:15, ((wss[1] - wss)*100)/wss[1], type = "b", xlab = "Number of clusters",
      ylab = "Amount of total variance in %", ylim = c(0,100), main = "Elbowplot")
    ##Scree-plot
    plot(1:15, wss, type = "b", xlab = "Number of clusters",
      ylab = "Within groups sum of squares", main = "Screeplot")
    #plots also with:
    #silhouette
    fviz_nbclust(num.data, kmeans, method = "silhouette") +
      labs(subtitle = "Silhouette method")
    # Gap statistic
    nb0 = fviz_nbclust(num.data, kmeans, nstart = 25,  method = "gap_stat", nboot = 100, verbose = FALSE) +
      labs(subtitle = "Gap statistic method")
    #all Nbclust indices considering:
    nb = invisible(NbClust(num.data, distance = "euclidean", min.nc = 2,
      max.nc = 10, method = "kmeans"))
    res.oa = fviz_nbclust(nb)
    optim.amount = as.numeric(sub("\\D+", "", res.oa$labels$title))
    ###### clusters for combinations: ######
    comb.cluster.list = apply(combinations, 1, function(x) {
      cols = colnames(num.data)[x]
      nb = NbClust(num.data[, x], distance = "euclidean", min.nc = 2,
        max.nc = 10, method = "kmeans")
      res.oa = fviz_nbclust(nb)
      optim.amount = as.numeric(sub("\\D+", "", res.oa$labels$title))
      ##apply cluster with optim:

      ##store results:
    })
  }
out.list = list()
return(out.list)
}
