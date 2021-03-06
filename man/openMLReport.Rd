% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/openMLReport.R
\name{openMLReport}
\alias{openMLReport}
\title{Creates a full report for OpenML data sets}
\usage{
openMLReport(data.id, reports = c("Basic", "CatSum", "NumSum", "Corr",
  "Cluster", "PCA", "MDS", "FA"), m.par.vals = list())
}
\arguments{
\item{data.id}{[\code{integer(1)}]\cr
data.id is the id from openML datasets see
\code{\link[OpenML]{getOMLDataSet}}}

\item{reports}{[\code{character()}]\cr
The report types used on the dataset. Default will use all
available reports if they are applicable}

\item{m.par.vals}{[\code{list()}]\cr
A names list containing parameters which will be passed to
the corressponding report function. [WIP]}
}
\description{
Imports a dataset from the openML data base and
creates a rmd report file
}
\examples{
# Report for the iris dataset
\dontrun{
openMLReport(61L)
openMLReport(61L, "Cluster",
  m.par.vals = list(Cluster = list(method = "cluster.kmeans",
    par.vals = list(algorithm = "MacQueen"))))
}
}
