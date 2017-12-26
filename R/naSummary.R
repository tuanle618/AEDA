#' Characterizes only variables of a data set with missing values. So, missing values are painted
#' black, while other observations keep white.
#'
#' @param data [\code{data.frame}]\cr
#'   Data to summarize. Columns can be of type numeric, integer, logical, factor or character.
#'   Characters and logicals will be treated as factors.
#' @param show.plot [\code{logical(1)}]\cr
#'   A logic value set to \code{FALSE} as default.
#' @param show.result [\code{logical(1)}]\cr
#'   A logic value set to \code{FALSE} as default.
#' @param margin.left [\code{numeric(1)}]\cr
#'   A numeric value which defines the margin size of the left. For more information see \link[graphics]{par}.
#' @param report.task [\code{ReportTaskObj}]\cr
#'   A Report Task Object.
#' @return A [\code{naSumObj}] with Names of the variables with their frequency of missing values and two additional plots
#'   which shows the position of the missing values (color = black) for each variable with NAs and the number of missing values as a bar plot
#' @examples
#'  data("airquality")
#'  #create new columns
#'  set.seed(1217)
#'  airquality$new1 = sample(1:nrow(airquality))
#'  airquality$new2 = rnorm(nrow(airquality))
#'  #add more NAs
#'  idx = sample(1:nrow(airquality), size = 15)
#'  airquality[idx, c("new1", "new2")] = NA
#'  idx2 = sample(1:nrow(airquality), size = 7)
#'  airquality[idx2, "Temp"] = NA
#'  #create the NA summary
#'  na.summary = summaryNA(data = airquality, show.plot = FALSE, show.result = FALSE, margin.left = 4, report.task = NULL)
#'  #plot the object through print
#'  na.summary
#'  #retrieve the elements through the components
#'  na.summary$nsum
#'  na.summary$image()
#'
#' @export
#' @import checkmate
#' @import BBmisc
#' @import ggplot2
#' @import grid
#' @import gridBase
#' @title Giving a NA summary and an image of a data with missing values

summaryNA  = function(data, show.plot = FALSE, show.result = FALSE, margin.left = 4, report.task = NULL){
  assertDataFrame(data)
  assertLogical(show.plot)
  assertLogical(show.result)
  assertNumeric(margin.left, lower = 2.5, upper = 5.5, len = 1L)

  num = as.numeric(which(apply(is.na(data), 2, any)))

  if (length(num) > 0) {

    na.df = data.frame("feature" = names(data),
      "num_missing" = sapply(data, function(x) {sum(is.na(x))}),
      "pct_missing" = sapply(data, function(x) {sum(is.na(x))}) / nrow(data), row.names = NULL)
    na.df = na.df[with(na.df, order(-num_missing)), ]

    if (show.result) {
      cat("In total there are:", sum((na.df$num_missing)), "NAs in the dataset:", deparse(substitute(data)), "\n")
      print(na.df)
    }
    #get the data containing the columns with NAs
    data.new = data[, num, drop = FALSE]
    #flag the NAs as 1
    color = apply(data.new, 2, function(x) as.integer(is.na(x)))
    #order column from largest NAs to lower
    color = color[, as.character(subset(na.df, num_missing > 0)[["feature"]])]
    data.new = data.new[, as.character(subset(na.df, num_missing > 0)[["feature"]])]
    #create image function :
    image.code = function() {
      image(color, col = c("white", "black"), yaxt = "n", xlab = "Index from observation", xaxt = "n")
      par(mar = c(5, margin.left, 4, 2) + 0.1)
      abline(v = -0.001)
      abline(h = 1.015)

      if (length(num) == 1) {
        y.type = 0
      } else {
        y.type = 0:(ncol(data.new) - 1) / (length(data.new) - 1)
      }

      axis(2, labels = colnames(data.new), at = y.type, las = 2)
    }

    #create na.ggplot
    na.ggplot = ggplot(subset(na.df, num_missing > 0), aes_string(x = "feature", y = "num_missing")) +
      geom_bar(stat = "identity", colour = "black", alpha = 0.4) +
      geom_text(aes(label = paste0(round(100 * pct_missing, 0), "%")), hjust = -0.15, size = 3.5) +
      scale_y_continuous(labels = waiver()) +
      theme(legend.position = "bottom") + coord_flip() +
      xlab("Features") + ylab("Number of missing values")

    if (show.plot) {
      #Create figure window and layout
      plot.new()
      grid.newpage()
      pushViewport(viewport(layout = grid.layout(2, 1)))

      #Draw ggplot
      pushViewport(viewport(layout.pos.row = 1))
      print(na.ggplot, newpage = FALSE)
      popViewport()

      #Draw base plot
      pushViewport(viewport(layout.pos.row = 2))
      par(fig = gridFIG(), new = TRUE)
      image.code()
      popViewport()
    }
    env = new.env(parent = emptyenv())
    env$color = color
    env$data.new = data.new
    env$margin.left = margin.left
    env$num = num
    makeS3Obj("naSumObj", na.df = na.df, data = data, dataset.name = deparse(substitute(data)), env = env,
      image = function() {
        image(env$color, col = c("white", "black"), yaxt = "n")
        par(mar = c(5, env$margin.left, 4, 2) + 0.1)
        abline(v = -0.001)
        abline(h = 1.015)
        if (length(env$num) == 1) {
          #insert y.type into environment
          env$y.type = 0
        } else {
          env$y.type = 0:(ncol(env$data.new) - 1) / (length(env$data.new) - 1)
        }
        axis(2, labels = colnames(env$data.new), at = env$y.type, las = 2)
        #remove y.type from the environment
        rm(y.type, envir = env)
      }, ggplot = na.ggplot)

  }
  else{
    if (show.result) cat("There are no missing values in the dataset: ", deparse(substitute(data)), "\n")
    makeS3Obj("naSumObj", na.df = NULL, data = data, dataset.name = deparse(substitute(data)), env = NULL, image = NULL,
      ggplot = NULL)
  }
}

#' @export
# Print function for naSumObj
print.naSumObj = function(x, ...) {
  if (is.null(x$na.df)) {
    catf("There are no missing values in the dataset: %s", x$dataset.name)
  } else {
    cat("In total there are", sum(x$na.df$num_missing), "NAs in the dataset:", x$dataset.name, "\n")
    print(x$na.df)
    cat("Printing image object with NAs according to observation index: \n")
    image(x$env$color, col = c("white", "black"), yaxt = "n", xaxt = "n")
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
    cat("Printing ggplot object according to number of missing values: \n")
    print(x$ggplot)
  }
}
