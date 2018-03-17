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
#' @import NbClust
#' @import factoextra
#' @import fpc
#' @import dbscan
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
      stand = FALSE, ellipse.type = "norm", ggtheme = theme_classic())
    ##save results for all numeric data:
    cluster.all = list(cluster.diag = list(nb.silh = nb.silh, nb.gap = nb.gap, nb.wss = nb.wss),
      cluster.res = optim.cluster,
      cluster.plot = pca.cluster.plot)
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
        stand = FALSE, ellipse.type = "norm", show.clust.cent = TRUE, ggtheme = theme_classic())
      ##store results:
      list(cluster.cols = cols,
        cluster.diag = list(nb.gap = nb.gap, nb.wss = nb.wss, nb.silh = nb.silh),
        cluster.res = optim.cluster,
        cluster.plot = cluster.plot)
    })

    ##Hierarchical clustering: ##work on all numeric data:
  } else if (is.element(method, c("cluster.h", "cluster.agnes", "cluster.diana"))) {
    res.dist = get_dist(num.data, stand = TRUE, method = "euclidean")
    plot.dist = fviz_dist(res.dist,
      gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
    #http://www.r-tutor.com/gpu-computing/clustering/hierarchical-cluster-analysis
    #check for performance
    if (method == "cluster.h") {
      cluster.method = "hclust"
    } else if (method == "cluster.agnes") {
      cluster.method = "agnes"
    } else if (method == "cluster.diana") {
      cluster.method = "diana"
    }
    out.clust = eclust(x = num.data, FUNcluster = cluster.method, verbose = FALSE, k = 4)
    # Visualize using factoextra
    # Cut in 4 groups and color by groups
    dend.plot = fviz_dend(out.clust, k = out.clust$nbclust, # Cut in 4 groups
      cex = 0.5, # label size
      k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
      color_labels_by_k = TRUE, # color labels by groups
      rect = TRUE, # Add rectangle around groups,
      show_labels = TRUE
    )
    #silhouette plot:
    silh.plot = fviz_silhouette(out.clust, print.summary = FALSE)
    #Which samples have negative silhouette? To what cluster are they closer?
    # Silhouette width of observations
    sil = out.clust$silinfo$widths[, 1:3]
    # Objects with negative silhouette
    neg.sil.index = which(sil[, "sil_width"] < 0)
    neg.sil.out = sil[neg.sil.index, , drop = FALSE]
    #save results, dont save dist.matrix and neg.sil.out
    cluster.all = list(clust.diag = list(dist.plot = plot.dist, silh.plot = silh.plot),
      cluster.res = out.clust,
      cluster.plot = dend.plot)
    #initialize empty list for combinations
    comb.cluster.list = list()
  } else if (method == "cluster.dbscan") {
    ###### all numeric colums together: ######
    #apply db scan algorithm, from dbscan pkg since faster implementation
    db.cluster = dbscan::dbscan(num.data, eps = 0.15, minPts = 5)
    #plot results
    db.plot = fviz_cluster(db.cluster, data = num.data, stand = FALSE,
      ellipse = FALSE, show.clust.cent = TRUE,
      geom = "point", palette = "jco", ggtheme = theme_classic())
    #mostly no db-cluster because if dim(X) > 2, apply PCA.. No Structure
    #save results
    cluster.all = list(cluster.diag = list(),
      cluster.res = db.cluster,
      cluster.plot = db.plot)
    ###### apply on combinations ######
    comb.cluster.list = apply(combinations, 2, function(x) {
      #print(x)
      cols = colnames(num.data)[x]
      #apply db scan algorithm
      db.cluster = dbscan::dbscan(num.data[, x], eps = 0.15, minPts = 5)
      #plot results
      db.plot = fviz_cluster(db.cluster, data = num.data[, x], stand = FALSE,
        ellipse = FALSE, show.clust.cent = TRUE,
        geom = "point", palette = "jco", ggtheme = theme_classic())
      #save results
      list(cluster.cols = cols,
        cluster.diag = list(),
        cluster.res = db.cluster,
        cluster.plot = db.plot)
    })
  } else if (method == "cluster.kkmeans") {
      ###### all numeric colums together: ######
      #apply kernel k-means algorithm ###DEFAULT centers?? ###ADD additionals params ?
      ###include PCA on num.data::
      pca.data = prcomp(x = num.data, rank. = 2L)
      kernel.cluster = invisible(kkmeans(x = pca.data$x, centers = 2L))
      var.pca1 = pca.data$sdev[1]/sum(pca.data$sdev)
      var.pca2 = pca.data$sdev[2]/sum(pca.data$sdev)
      #plot results ##DO ggplot manually
      proc.data = as.data.frame(cbind(pca.data$x, cluster = kernel.cluster@.Data))
      proc.data$cluster = as.factor(proc.data$cluster)
      tmp.foo = as.data.frame(kernel.cluster@centers)
      colnames(tmp.foo) = c("PC1", "PC2")
      tmp.foo$cluster = c(1,2); tmp.foo$cluster = as.factor(tmp.foo$cluster)
      kernel.plot = ggplot(proc.data, aes(x = PC1, y = PC2, color = cluster)) + geom_point(shape = 1)
      ##add cluster means
      kernel.plot = kernel.plot + geom_point(data = tmp.foo, aes(x = PC1, y = PC2), size = 3)
      ##add title, change axis with variance info:
      kernel.plot = kernel.plot + ggtitle("Kernel K-Means Result") + labs(x = paste("PC1 explaining", round(var.pca1*100, 2), "% of Variance"),
        y = paste("PC2 explaining", round(var.pca2*100, 2), "% of Variance")) + theme_classic()
      #save results
      cluster.all = list(cluster.diag = list(),
        cluster.res  = kernel.cluster,
        cluster.plot = kernel.plot)
      ###### apply on combinations ######
      comb.cluster.list = apply(combinations, 2, function(x) {
        #print(x)
        cols = colnames(num.data)[x]
        #apply kernel k-means algorithm
        kernel.cluster = invisible(kkmeans(x = num.data[, x], centers = 2L))
        proc.data = as.data.frame(cbind(num.data[, x], cluster = kernel.cluster@.Data))
        proc.data$cluster = as.factor(proc.data$cluster)
        tmp.foo = as.data.frame(kernel.cluster@centers)
        colnames(tmp.foo) = c(colnames(proc.data)[1], colnames(proc.data)[2])
        tmp.foo$cluster = c(1,2); tmp.foo$cluster = as.factor(tmp.foo$cluster)
        #plot results
        kernel.plot = ggplot(proc.data, aes_string(x = colnames(proc.data)[1], y = colnames(proc.data)[2], color = "cluster")) + geom_point(shape = 1) + ggtitle("Kernel K-Means Result")
        kernel.plot = kernel.plot + geom_point(data = tmp.foo, aes_string(x = colnames(proc.data)[1], y = colnames(proc.data)[2]), size = 3) + theme_classic()
        #save results
        list(cluster.cols = cols,
          cluster.diag = list(),
          cluster.res = kernel.cluster,
          cluster.plot =  kernel.plot)
      })
  } else if (method == "cluster.mod") {
    ###### all numeric colums together: ######
      # Apply Model-Based-Clustering
      mod.cluster = Mclust(num.data, verbose = FALSE)
      # BIC values used for choosing the number of clusters
      plot.mod.bic = fviz_mclust(mod.cluster, "BIC", palette = "jco", ggtheme = theme_classic())
      # Classification: plot showing the clustering
      plot.mod.cluster = fviz_mclust(mod.cluster, "classification", geom = "point",
        pointsize = 1.5, palette = "jco", ggtheme = theme_classic())
      # Classification uncertainty
     plot.mod.uncert =  fviz_mclust(mod.cluster, "uncertainty", palette = "jco", ggtheme = theme_classic())
      #Save results into list
     cluster.all = list(cluster.diag = list(plot.mod.bic = plot.mod.bic, plot.mod.uncert = plot.mod.uncert),
       cluster.res = mod.cluster,
       cluster.plot = plot.mod.cluster)
     ###### apply on combinations ######
     comb.cluster.list = apply(combinations, 2, function(x) {
       cols = colnames(num.data)[x]
       # Apply Model-Based-Clustering
       mod.cluster = Mclust(num.data[, x], verbose = FALSE)
       # BIC values used for choosing the number of clusters
       plot.mod.bic = fviz_mclust(mod.cluster, "BIC", palette = "jco", ggtheme = theme_classic())
       # Classification: plot showing the clustering
       plot.mod.cluster = fviz_mclust(mod.cluster, "classification", geom = "point",
         pointsize = 1.5, palette = "jco", ggtheme = theme_classic())
       # Classification uncertainty
       plot.mod.uncert =  fviz_mclust(mod.cluster, "uncertainty", palette = "jco", ggtheme = theme_classic())
       list(cluster.cols = cols,
         cluster.diag = list(plot.mod.bic = plot.mod.bic, plot.mod.uncert = plot.mod.uncert),
         cluster.res = mod.cluster,
         cluster.plot = plot.mod.cluster)
     })
    }
out.list = list(cluster.all = cluster.all, comb.cluster.list = comb.cluster.list)
return(out.list)
}
