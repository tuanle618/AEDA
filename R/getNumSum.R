getNumSum = function(data, features) {
  num.data = subset(data, select = features)
  #for the next calculations NAs will be removed
  no.obs = apply(num.data, 2, function(x) sum(!is.na(x)))
  nas = apply(num.data, 2, function(x) sum(is.na(x)))
  mean = apply(num.data, 2, function(x) mean(x, na.rm = TRUE))
  kurtosis = apply(num.data, 2, function(x) moments::kurtosis(x, na.rm = TRUE))
  skewness = apply(num.data, 2, function(x) moments::skewness(x, na.rm = TRUE))
  sd = apply(num.data, 2, function(x) sd(x, na.rm = TRUE))
  min = apply(num.data, 2, function(x) min(x, na.rm = TRUE))
  q0.25 = apply(num.data, 2, function(x) stats::quantile(x, probs = 0.25, na.rm = TRUE))
  median = apply(num.data, 2, function(x) stats::median(x, na.rm = TRUE))
  q0.75 = apply(num.data, 2, function(x) stats::quantile(x, probs = 0.75, na.rm = TRUE))
  max = apply(num.data, 2, function(x) max(x, na.rm = TRUE))
  range = max - min
  iqr = apply(num.data, 2, function(x) stats::IQR(x, na.rm = TRUE))
  no.zero = apply(num.data, 2, function(x) length(which(x == 0)))
  no.unique = apply(num.data, 2, function(x) length(unique(x)))
  #num.sum.df =
}

