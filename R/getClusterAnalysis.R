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
  ##### http://www.sthda.com/english/wiki/print.php?id=239 #####
  #select numeric data
  set.seed(random.seed)
  num.data = data[, num.features]
  if (scale.num.data) num.data = scale(num.data)
  #tupel combinations
  combinations = combn(x = 1:ncol(num.data), m = 2)

  ##K-Means or PAM:
  if (is.element(method, c("cluster.kmeans", "cluster.pam"))) {
    ###### all numeric colums together: ######
    ##asign function to hand over to fviz_nbclust
    if (method == "cluster.kmeans") {
      cluster.method = kmeans
    } else cluster.method = pam

    ###Optimal amount of clusters:
    ##work with gg-plot-objects provided in factorextra and Nbclust pkg

    # Silhouette-method
    nb.silh = fviz_nbclust(x = num.data, FUNcluster = cluster.method, method = "silhouette") +
      labs(subtitle = "Silhouette method")
    #extract optim amount of clusters according to silhouette method
    optim.silh = as.integer(as.character(nb.silh$data[which.max(nb.silh$data$y), 1]))

    # Gap-method
    nb.gap = fviz_nbclust(x = num.data, FUNcluster = cluster.method,  method = "gap_stat", nboot = 100, verbose = FALSE) +
      labs(subtitle = "Gap statistic method")
    # ToDo: function to extract optim number of cluster. Subsection 'Algorithm 5.4 on website'

    nb.wss = fviz_nbclust(num.data, cluster.method, method = "wss") +
      labs(subtitle = "Elbow method")

    ##do the cluster with optim silhouette:
    if (method == "cluster.kmeans") {
      optim.cluster = kmeans(x = num.data, centers = optim.silh, iter.max = 30L)
    } else optim.cluster = pam(x = num.data, k = optim.silh)
    ##plot cluster: NOTE: PCA with 2principal components applied:
    pca.cluster.plot = fviz_cluster(optim.cluster, data = num.data, geom = "point",
      stand = FALSE, ellipse.type = "norm")
    ##save results for all numeric data:
    cluster.all = list(nb.silh = nb.silh, nb.gap = nb.gap, nb.wss = nb.wss,
      optim.amount = optim.silh, optim.cluster = optim.cluster, pca.cluster.plot = pca.cluster.plot)

    ###### clusters for combinations: ######
    comb.cluster.list = apply(combinations, 2, function(x) {
      #print(x)
      cols = colnames(num.data)[x]
      #placeholder: NbClust takes too much time
      if (FALSE) {
        nb = invisible(NbClust(num.data[, x], distance = "euclidean", min.nc = 2,
          max.nc = 10, method = "kmeans"))
        res.oa = fviz_nbclust(nb)
        optim.amount = as.numeric(sub("\\D+", "", res.oa$labels$title))
      }
      ##gap:
      nb.gap = fviz_nbclust(x = num.data[, x], FUNcluster = cluster.method,  method = "gap_stat", nboot = 100, verbose = FALSE) +
        labs(subtitle = "Gap statistic method")
      ##elbow:
      nb.wss = fviz_nbclust(num.data[, x], FUNcluster = cluster.method, method = "wss") +
        labs(subtitle = "Elbow method")
      ##silhouette:
      nb.silh = fviz_nbclust(x = num.data[, x], FUNcluster = cluster.method, method = "silhouette") +
        labs(subtitle = "Silhouette method")
      #extract optim amount of clusters according to silhouette method
      optim.silh = as.integer(as.character(nb.silh$data[which.max(nb.silh$data$y), 1]))
      ##do the cluster with optim silhouette:
      if (method == "cluster.kmeans") {
        optim.cluster = kmeans(x = num.data[, x], centers = optim.silh, iter.max = 30L)
      } else optim.cluster = pam(x = num.data[, x], k = optim.silh)
      ##ggplot clusterw
      cluster.plot = fviz_cluster(optim.cluster, data = num.data[, x], geom = "point",
        stand = FALSE, ellipse.type = "norm")
      ##store results:
      list(names = cols, nb.gap = nb.gap, nb.wss = nb.wss, nb.silh = nb.silh, optim.amount = optim.silh,
        optim.cluster = optim.cluster, cluster.plot = cluster.plot)
    })

    ##Hierarchical clustering:
  } else if (method == "cluster.h") {



  }
out.list = list(cluster.all = cluster.all, comb.cluster.list = comb.cluster.list)
return(out.list)
}
