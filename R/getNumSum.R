#' @title Computes the components and plots for the Numeric Summary
#'
#' @description
#'  getNumSum applies function over columns in order to extract location- and scale parameters.
#'  In Addition to that, for each column, a ggplot object will be implemented.
#'
#' @param data [\code{data.frame}]\cr
#'   A Dataframe with different variables.
#' @param features [\code{character(length(numeric.features))}]\cr
#'   A character vector with length of the number of numeric features in the dataset.
#'   This will be computed automatically when calling this function.
#' @param target [\code{character(1)}]\cr
#'   The target column
#' @param geom.hist.args [\code{list()}] \cr
#'  Other arguments to be passed to \link[ggplot2]{geom_histogram}.
#'  Inserted in \link[AEDA]{makeNumSumTask}
#' @param geom.dens.args [\code{list()}] \cr
#'  Other arguments to be passed to \link[ggplot2]{geom_density}.
#'  Inserted in \link[AEDA]{makeNumSumTask}
#' @param geom.box.args [\code{list()}] \cr
#'  Other arguments to be passed to \link[ggplot2]{geom_boxplot}.
#'  Inserted in \link[AEDA]{makeNumSumTask}
#' @return [\code{list()}]
#'   A list containing the numeric summary and ggplot for each numeric column
#' @import moments
#' @import stats
#' @import checkmate
#'

getNumSum = function(data, features, target, geom.hist.args, geom.dens.args, geom.box.args) {
  assertDataFrame(data)
  if (!is.null(target)) assertCharacter(target, len = 1L)
  assertCharacter(features, min.len = 1L, min.chars = 1L)

  num.data = subset(data, select = features)
  if (any(is.na(num.data))) warning("The data set contains NAs. These values will be removed in the further calculations")
  no.obs = apply(num.data, 2, function(x) sum(!is.na(x)))
  nas = apply(num.data, 2, function(x) sum(is.na(x)))
  nas.perc = nas / nrow(num.data)
  mean = apply(num.data, 2, function(x) mean(x, na.rm = TRUE))
  kurtosis = apply(num.data, 2, function(x) kurtosis(x, na.rm = TRUE))
  skewness = apply(num.data, 2, function(x) skewness(x, na.rm = TRUE))
  sd = apply(num.data, 2, function(x) sd(x, na.rm = TRUE))
  min = apply(num.data, 2, function(x) min(x, na.rm = TRUE))
  quantiles = apply(num.data, 2, function(x) quantile(x, probs = c(.01, .05, .1, .25, .5, .75, .9, .95, .99), na.rm = TRUE))
  max = apply(num.data, 2, function(x) max(x, na.rm = TRUE))
  range = max - min
  iqr = apply(num.data, 2, function(x) IQR(x, na.rm = TRUE))
  l.bound = quantiles[4, ] - 1.5 * iqr
  u.bound = quantiles[6, ] + 1.5 * iqr
  no.outliers = sapply(colnames(num.data), FUN = function(x) length(which(num.data[[x]] < l.bound[x] | num.data[[x]] > u.bound[x])))
  no.zero = apply(num.data, 2, function(x) length(which(x == 0)))
  no.unique = apply(num.data, 2, function(x) length(unique(x)))
  num.sum.df = round(t(as.data.frame(rbind(no.obs, nas, nas.perc, mean, kurtosis, skewness, sd, min, quantiles, max, range, iqr,
    l.bound, u.bound, no.outliers, no.zero, no.unique))), digits = 3)
  tt = target
  plot.distr.list = lapply(features, function(x) {
    plotFeatDistr(data = data, target = tt, col = x, geom.hist.args = geom.hist.args, geom.dens.args = geom.dens.args)
  })
  plot.box.list = lapply(features, function(x) do.call(plotBox, append(list(data = data, target = target, col = x), geom.box.args)))
  plot.box.list = split.list.gg.helper(plot.box.list)
  plot.hist.list = lapply(features, function(x) do.call(plotHist, append(list(data = data, target = target, col = x), geom.hist.args)))
  plot.hist.list = split.list.gg.helper(plot.hist.list)

  names(plot.distr.list) = names(plot.box.list) = names(plot.hist.list) = row.names(num.sum.df)

  #merge num.sum.df with plotlist
  merged.list = vector(mode = "list", length = nrow(num.sum.df))
  names(merged.list) = row.names(num.sum.df)
  for (col in names(merged.list)) {
    merged.list[[col]] = list(summary = num.sum.df[col, ], plot.distr = plot.distr.list[[col]], plot.hist = plot.hist.list[[col]],
      plot.box = plot.box.list[[col]])
  }
  out.list = list(num.sum.df = num.sum.df, merged.list = merged.list)
  return(out.list)
}
