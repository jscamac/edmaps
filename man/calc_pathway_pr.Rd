% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_pathway_pr.R
\name{calc_pathway_pr}
\alias{calc_pathway_pr}
\title{Calculate the pathway-specific probability of pest arrival}
\usage{
calc_pathway_pr(EE, rast, outfile, return_rast = TRUE)
}
\arguments{
\item{EE}{\code{data.frame} object obtained from \code{\link[=calc_EE]{calc_EE()}}.}

\item{rast}{Raster object or character path to raster file containing
dispersal weights.}

\item{outfile}{Character. Output raster file path. If not provided, the
\code{RasterLayer} will be returned to R.}

\item{return_rast}{Logical. Should the \code{RasterLayer} be returned to R?
Ignored if \code{outfile} is not provided.}
}
\value{
If \code{outfile} is specified, the resulting \code{RasterLayer} is
saved to \code{outfile}. If \code{return_rast} is \code{TRUE} or
\code{outfile} is not specified, the resulting \code{RasterLayer} is
returned, otherwise \code{NULL} is returned invisibly.
}
\description{
Calculate the pathway-specific probability of pest arrival for each raster
cell.
}
