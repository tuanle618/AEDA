#' Characterizes only variables of a data set with missing values. So, missing values are painted
#' black, while other observations keep white.
#'
#' @param data [\code{data.frame}]\cr
#'   Data to summarize. Columns can be of type numeric, integer, logical, factor or character.
#'   Characters and logicals will be treated as factors.
#' @param show.plot [\code{logical(1)}]\cr
#'   A logic value set to \code{FALSE} as default.
#'  @param show.result [\code{logical(1)}]\cr
#'   A logic value set to \code{FALSE} as default.
#' @param margin.left [\code{numeric(1)}]\cr
#'   A numeric value which defines the margin size of the left. For more information see \link[graphics]{par}.
#' @param report.task [\code{ReportTaskObj}]\cr
#'   A Report Task Object.
#' @return A \code{naSumObj} with Names of the variables with their frequency of missing values and an additional plot
#'   which shows the position of the missing values (color = black) for each variable with NAs.
#'
#' @export
#' @import checkmate
#' @import BBmisc
#' @title Giving an image of a data with missing values

summaryNA  = function(data, show.plot = FALSE, show.result = FALSE, margin.left = 4, report.task = NULL){
  assertDataFrame(data)
  assertLogical(show.plot)
  assertLogical(show.result)
  assertNumeric(margin.left, lower = 2.5, upper = 5.5, len = 1L)

  num = as.numeric(which(apply(is.na(data), 2, any)))

  if (length(num) > 0){

    na.summary = numeric(length(num))
    names(na.summary) = colnames(data)[num]
    na.summary = colSums(is.na(data[, num, drop = FALSE]))
    if (show.result){
      cat("In total there are:", sum(na.summary), "NAs in the dataset:", deparse(substitute(data)), "\n")
      print(na.summary)
    }
    #get the data containing the columns with NAs
    data.new = data[, num, drop = FALSE]
    #flag the NAs as 1
    color = apply(data.new, 2, function(x) as.integer(is.na(x)))

    #create image function :
    image.code = function() {
      image(color, col = c("white", "black"), yaxt = "n")
      par(mar = c(5, margin.left, 4, 2) + 0.1)
      abline(v = -0.001)
      abline(h = 1.015)

      if (length(num) == 1){
        y.type = 0
      } else {
        y.type = 0:(ncol(data.new) - 1) / (length(data.new) - 1)
      }

      axis(2, labels = colnames(data.new), at = y.type, las = 2)
    }

    if (show.plot){
      image.code()
    }
    env = new.env(parent = emptyenv())
    env$color = color
    env$data.new = data.new
    env$margin.left = margin.left
    env$num = num
    makeS3Obj("naSumObj", nas = na.summary, data = data, dataset.name = deparse(substitute(data)), env = env)

  }
  else{
    cat("There are no missing values in the dataset: ", deparse(substitute(data)), "\n")
    makeS3Obj("naSumObj", nas = NULL, data = data, dataset.name = deparse(substitute(data)), env = NULL)
  }
}

#' @export
# Print function for naSumObj
print.naSumObj = function(x, ...) {
  if (is.null(x$nas)) {
    catf("There are no missing values in the dataset: %s", x$dataset.name)
  } else {
    cat("In total there are", sum(x$nas), "NAs in the dataset:", x$dataset.name, "\n")
    print(x$nas)
    image(x$env$color, col = c("white", "black"), yaxt = "n")
    par(mar = c(5, x$env$margin.left, 4, 2) + 0.1)
    abline(v = -0.001)
    abline(h = 1.015)
    if (length(x$env$num) == 1) {
      #insert y.type into environment
      x$env$y.type = 0
    } else {
      x$env$y.type = 0:(ncol(x$env$data.new) - 1) / (length(x$env$data.new) - 1)
    }
    axis(2, labels = colnames(x$env$data.new), at = x$env$y.type, las = 2)
    #remove y.type from the environment
    rm(y.type, envir = x$env)
  }
}
