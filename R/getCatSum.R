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
#'   The target column
#' @return [\code{list()}]
#'   A list containing the categorical summary and ggplot for each categorical column
#' @import checkmate
#' @import utils
#' @export

getCatSum = function(data, features, target) {
  assertDataFrame(data)
  if (!is.null(target)) assertCharacter(target, len = 1L)
  assertCharacter(features, min.len = 1L, min.chars = 1L)

  cat.data = subset(data, select = features)

  freq = apply(cat.data, 2, table)
  rel.freq = lapply(freq, prop.table)
  nas = as.list(apply(cat.data, 2, function(x) sum(is.na(x))))
  comb = combn(features, m = 2)
  contg.list = vector("list", length = ncol(comb))
  for (col in seq_len(ncol(comb))) {
    contg.list[[col]] = table(cat.data[, comb[, col]])
  }
  rel.contg.list = lapply(contg.list, prop.table)


  return(NULL)
}
