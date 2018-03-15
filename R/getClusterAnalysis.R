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
    plotRMD = FALSE #placeholder
    if (plotRMD) {
      for (i in 2:15) wss[i] = sum(kmeans(num.data,
        centers = i, iter.max = 20L)$withinss)
      ##Elbowplot
      plot(1:15, ((wss[1] - wss)*100)/wss[1], type = "b", xlab = "Number of clusters",
        ylab = "Amount of total variance in %", ylim = c(0,100), main = "Elbowplot")
      ##Scree-plot
      plot(1:15, wss, type = "b", xlab = "Number of clusters",
        ylab = "Within groups sum of squares", main = "Screeplot")
    }
    ##rather work with gg-plot-objects provided in factorextra and Nbclust pkg
    #plots also with:
    #silhouette
    nb.silh = fviz_nbclust(num.data, kmeans, method = "silhouette") +
      labs(subtitle = "Silhouette method")
    #extract optim amount of clusters according to silhouette method
    optim.silh = as.integer(as.character(nb.silh$data[which.max(nb.silh$data$y), 1]))
    # Gap statistic
    nb.gap = fviz_nbclust(num.data, kmeans, nstart = 25,  method = "gap_stat", nboot = 100, verbose = FALSE) +
      labs(subtitle = "Gap statistic method")
    #all Nbclust indices considering: !!! takes long to execute... !!!
    nb.all = invisible(NbClust(num.data, distance = "euclidean", min.nc = 2,
      max.nc = 10, method = "kmeans"))
    res.oa = fviz_nbclust(nb.all)
    optim.amount = as.numeric(sub("\\D+", "", res.oa$labels$title))

    ##do the cluster with optim amount:
    optim.cluster = kmeans(num.data, centers = optim.amount)

    ##save results for all numeric data:
    cluster.all = list(nb.silh = nb.silh, nb.gap = nb.gap, nb.all = nb.all, res.oa = res.oa,
      optim.amount = optim.amount, optim.cluster = optim.cluster) ##include optim.amount.silh?!

    ###### clusters for combinations: ######
    comb.cluster.list = apply(combinations, 2, function(x) {
      #print(x)
      cols = colnames(num.data)[x]
      #placeholder
      if (FALSE) {
        nb = invisible(NbClust(num.data[, x], distance = "euclidean", min.nc = 2,
          max.nc = 10, method = "kmeans"))
        res.oa = fviz_nbclust(nb)
        optim.amount = as.numeric(sub("\\D+", "", res.oa$labels$title))
      }
      nb.silh = fviz_nbclust(num.data[, x], kmeans, method = "silhouette") +
      labs(subtitle = "Silhouette method")
      #extract optim amount of clusters according to silhouette method
      optim.silh = as.integer(as.character(nb.silh$data[which.max(nb.silh$data$y), 1]))
      ##apply cluster with optim:
      optim.cluster = kmeans(num.data[, x], centers = optim.silh)
      ##ggplot cluster
      cluster.plot = fviz_cluster(optim.cluster, data = num.data[, x], geom = "point",
        stand = FALSE, ellipse.type = "norm")
      ##store results:
      list(names = cols, nb.silh = nb.silh, optim.amount = optim.silh,
        optim.cluster = optim.cluster, cluster.plot = cluster.plot)
    })
  } else if (method == "cluster.h") {

  }
out.list = list(cluster.all = cluster.all, comb.cluster.list = comb.cluster.list)
return(out.list)
}
