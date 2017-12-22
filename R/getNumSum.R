getNumSum = function(data, features, target) {
  num.data = subset(data, select = features)
  #for the next calculations NAs will be removed
  no.obs = apply(num.data, 2, function(x) sum(!is.na(x)))
  nas = apply(num.data, 2, function(x) sum(is.na(x)))
  nas.perc = nas / nrow(num.data)
  mean = apply(num.data, 2, function(x) mean(x, na.rm = TRUE))
  kurtosis = apply(num.data, 2, function(x) moments::kurtosis(x, na.rm = TRUE))
  skewness = apply(num.data, 2, function(x) moments::skewness(x, na.rm = TRUE))
  sd = apply(num.data, 2, function(x) sd(x, na.rm = TRUE))
  min = apply(num.data, 2, function(x) min(x, na.rm = TRUE))
  quantiles = apply(num.data, 2, function(x) stats::quantile(x, probs = c(.01, .05, .1, .25, .5, .75, .9, .95, .99), na.rm = TRUE))
  max = apply(num.data, 2, function(x) max(x, na.rm = TRUE))
  range = max - min
  iqr = apply(num.data, 2, function(x) stats::IQR(x, na.rm = TRUE))
  l.bound = quantiles[4, ] - 1.5 * iqr
  u.bound = quantiles[6, ] + 1.5 * iqr
  no.outliers = sapply(colnames(num.data), FUN = function(x) length(which(num.data[[x]] < l.bound[x] | num.data[[x]] > u.bound[x])))
  no.zero = apply(num.data, 2, function(x) length(which(x == 0)))
  no.unique = apply(num.data, 2, function(x) length(unique(x)))
  num.sum.df = round(t(as.data.frame(rbind(no.obs, nas, nas.perc, mean, kurtosis, skewness, sd, min, quantiles, max, range, iqr,
    l.bound, u.bound, no.outliers, no.zero, no.unique))), digits = 3)

  tt = target
  plot.list = lapply(features, function(x) plotFeatDistr(data = num.data, target = tt, col = x))
  names(plot.list) = row.names(num.sum.df)

  #merge num.sum.df with plotlist
  merged.list = vector(mode = "list", length = nrow(num.sum.df))
  names(merged.list) = row.names(num.sum.df)
  for(col in names(merged.list)) {
    merged.list[[col]] = list(summary = num.sum.df[col,], plot = plot.list[[col]])
  }
  return(merged.list)
}
