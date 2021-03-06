% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotDens.R
\name{plotDens}
\alias{plotDens}
\title{Creates a density plot for one/ all numerical features(s) of a dataset.}
\usage{
plotDens(data, target, col = NULL, show.plot = FALSE, alpha = 0.4,
  colour = "black", ...)
}
\arguments{
\item{data}{[\code{data.frame}]\cr
Data.}

\item{target}{[\code{character(1)}]\cr
Target column. If no target is available in the dataset please insert \code{NULL}}

\item{col}{[\code{character(1)} | \code{integer(1)}]\cr
Selected feature from \code{data}. If all numerical features should be printed insert \code{NULL}.
Default value is \code{NULL}}

\item{show.plot}{[\code{logical(1)}]\cr
Logical whether the ggplot(s) should be displayed or not when executing this function.
Default is \code{FALSE}}

\item{alpha}{[\code{numeric(1)}]\cr
Default is \code{alpha = 0.4}}

\item{colour}{[\code{character(1)}]\cr
Default is  \code{colour = "black"}}

\item{\dots}{other arguments to be passed to \link[ggplot2]{geom_density}.}
}
\value{
A ggplot2 object. Print it to plot it. [WIP if col is null]
}
\description{
Creates a Density plot of a numerical feature or all numerical features of a dataset .
}
\examples{
 data("Boston", package = "MASS")
 gghistplot = plotDens(Boston, target = "medv", col = "age")
 gghistAllplot = plotDens(Boston, target = "medv", col = NULL)
}
