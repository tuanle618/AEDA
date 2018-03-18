#' @title Computes the components and plots for the Categorial Summary
#'
#' @description
#'  getCatSum calculates a few characteristics like frequency tables, contingency tables for categorical data and
#'  in Addition to that, for each column, a ggplot object will be implemented.
#'
#' @param data [\code{data.frame}]\cr
#'   A Dataframe with different variables.
#' @param features [\code{character(length(catergorical.features))}]\cr
#'   A character vector with length of the number of numeric features in the dataset.
#'   This will be computed automatically when calling this function.
#' @param target [\code{character(1)}]\cr
#' @param geombar.args [\code{list()}] \cr
#'  Other arguments to be passed to \link[ggplot2]{geom_bar}.
#'   The target column
#' @return [\code{list()}]
#'   A list containing the categorical summary and ggplot for each categorical column
#' @import checkmate
#' @import utils
#' @import stats
#' @export

getCatSum = function(data, features, target, geombar.args) {
  assertDataFrame(data)
  if (!is.null(target)) assertCharacter(target, len = 1L)
  assertCharacter(features, min.len = 1L, min.chars = 1L)

  cat.data = subset(data, select = features)

  freq = apply(cat.data, 2, table)
  rel.freq = lapply(freq, prop.table)
  nas = as.list(apply(cat.data, 2, function(x) sum(is.na(x))))
  if (length(features) >= 2) {
    comb = combn(features, m = 2)
    contg.list = vector("list", length = ncol(comb))
    for (col in seq_len(ncol(comb))) {
      contg.list[[col]] = table(cat.data[, comb[, col]])
    }
    rel.contg.list = lapply(contg.list, prop.table)
    rel.contg.list = lapply(rel.contg.list, addmargins)
    contg.list = lapply(contg.list, addmargins)
  } else {
    contg.list = NULL
    rel.contg.list = NULL
  }
  names(features) = features

  plot.list = lapply(features, function(x) do.call(plotBar, append(list(data = data, target = target, col = x), geombar.args)))
  plot.list = split.list.gg.helper(plot.list)
  out.list = list(freq = freq, rel.freq = rel.freq, nas = nas, contg.list = contg.list,
    rel.contg.list = rel.contg.list, plot.list = plot.list)
  return(out.list)
}
