#' @title Creates a Factor Analysis Task Object for numeric data
#'
#' @description
#' A Task encapsulates the Data with some additional information.\cr
#'
#' @param id [\code{character(1)}]\cr
#'   ID of the Task Object
#' @param data [\code{data.frame}]\cr
#'   A Dataframe with different variables
#' @param nfactors [\code{integer(1)}]\cr
#'   Number of factors to extract. If nothing is inserted the optimal \cr
#'   number of factors will be calculated according to parallel analysis.\cr
#'   For more information @seealso \code{\link[psych]{fa.parallel}}.\cr
#'   Default is \code{nfactors = NULL}
#' @param rotate [\code{character(1)}]\cr
#'   Defines the rotation method. Possible values are: \cr
#'   "none", "varimax", "quartimax", "bentlerT", "equamax", \cr
#'   "varimin", "geominT" and "bifactor",\cr
#'   "Promax", "promax", "oblimin", "simplimax",\cr
#'   "bentlerQ, "geominQ","biquartimin" and "cluster".\cr
#'   Default is \code{rotate = "oblimin"}.
#'   For more information @seealso \code{\link[psych]{fa}}
#' @param par.vals [\code{list}]\cr
#'   Additional arguments handled over to @seealso \code{\link[psych]{fa}}.\cr
#'   Default is empty list \code{par.vals = list()}
#' @param par.vals.parallel [\code{list}]\cr
#'   Additional arguments handed over to @seealso \code{\link[psych]{fa.parallel}}.\cr
#'   Default is \code{par.vals.parallel = list(plot = FALSE)}
#' @param show.NA.msg [\code{logical(1)}]\cr
#'   Logical whether to show missing values message\cr
#'   Default is \code{show.NA.msg = FALSE)}
#' @examples
#'  library(psych)
#'  data(bfi)
#'  #take small sample of size 200L:
#'  bfi_small = bfi[sample(seq_len(nrow(bfi)), size = 200L), ]
#'  my.FA.task = makeFATask(id = "bfi", data = bfi_small)
#' @return FATask Object
#' @import checkmate
#' @import BBmisc
#' @importFrom psych fa
#' @importFrom psych fa.parallel
#' @export
#'
makeFATask = function(id, data, nfactors = NULL, rotate = "oblimin",
  par.vals = list(), par.vals.parallel = list(plot = FALSE), show.NA.msg = FALSE){

  data.types = getDataType(data, target = NULL)
  num.features = c(data.types$num, data.types$int)
  if (length(num.features) < 2) {
    stop(paste("Your dataset only contains of",
      length(num.features),
      " numeric columns. Factor Analysis does not make sense"))
  }
  #Argument Checks
  assertCharacter(id, min.chars = 1L)
  assertDataFrame(data, col.names = "strict")

  #add warning for NAs:
  if (any(is.na(data)) & show.NA.msg) {
    message("The data set contains NAs.
      These values might removed in the further calculations.
      If so, another warning will be displayed.")
  }
  assertChoice(rotate, choices = c("none", "varimax", "quartimax", "bentlerT",
    "equamax", "varimin", "geominT", "bifactor",
    "Promax", "promax", "oblimin", "simplimax", "bentlerQ",
    "geominQ", "biquartimin", "cluster"))

  ##par.vals check:
  formals = formals(fa)
  for (arg in names(par.vals)) {
    if (!is.element(el = arg, set = names(formals))) {
      stop(paste(arg, "is not a parameter argument for MDS method:", method))
    }
  }

  ##par.vals.parallel check:
  formals = formals(fa.parallel)
  for (arg in names(par.vals.parallel)) {
    if (!is.element(el = arg, set = names(formals))) {
      stop(paste(arg, "is not a parameter argument for MDS method:", method))
    }
  }
  #if user inserted x = data we remove it
  if ("x" %in% names(par.vals.parallel)) {
    par.vals.parallel = par.vals.parallel[-which(names(par.vals.parallel) == "x")]
  }

  #extract numeric data:
  num.data = data[, num.features]
  #remove NAs
    if (any(is.na(num.data))) {
      #set the scores and missings to TRUE to handle NA in psych::fa()
      par.vals$scores = TRUE
      par.vals$missing = TRUE
      if (show.NA.msg == TRUE) {
        message("The numeric data contains NA. By default in psych::fa() imputation of median will be applied")
      }
    }

  #if number of factos is not inserted, calculate the optimal number of factors:
  if (is.null(nfactors)) {
    num.fact.analysis = do.call(fa.parallel, append(list(x = num.data), par.vals.parallel))
    nfactors = num.fact.analysis$nfact
  } else {
    num.fact.analysis = NULL
  }


  ####################
  # Encapsulate Data and Data Types into new env
  env = new.env(parent = emptyenv())
  env$num.data = num.data
  env$datatypes = data.types

  makeS3Obj("FATask",
    id = id,
    type = "FactAnalSummary",
    env = env,
    size = nrow(num.data),
    numdatatypes = list(numeric = env$datatypes$num, integer = env$datatypes$int),
    rotate = rotate,
    par.vals = par.vals,
    nfactors = nfactors,
    num.fact.analysis = num.fact.analysis
  )
  }

#' @export
# Print function for FATask Object
print.FATask = function(x, ...) {
  catf("Task: %s", x$id)
  catf("Observations: %i", x$size)
  catf("Amount Numeric Columns: %i", length(x$numdatatypes$numeric))
  catf("Amount Integer Columns: %i", length(x$numdatatypes$integer))
  catf("Selected Rotation: %s", x$rotate)
  catf("Amount of Factors: %s", x$nfactors)
  if (length(x$par.vals) != 0) {
    catf("Additional params for method: %i", length(x$par.vals))
    print(unlist(x$par.vals))
  }
}
