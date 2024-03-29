% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract_highest_ncells.R
\name{extract_highest_ncells}
\alias{extract_highest_ncells}
\title{Returns a raster with values for the n cells with highest establishment
likelihood}
\usage{
extract_highest_ncells(infile, n_cells, outfile, return_rast = FALSE)
}
\arguments{
\item{infile}{File path to a raster containing estimated likelihoods of
establishment or arrival.}

\item{n_cells}{Integer. The number of cells to return.}

\item{outfile}{Character. Output raster file path. If not provided, the
\code{RasterLayer} will be returned to R.}

\item{return_rast}{Logical. Should the \code{RasterLayer} be returned to R?
Ignored if \code{outfile} is not provided.}
}
\value{
If \code{outfile} is specified, the resulting \code{RasterLayer} is
saved as to that path. If \code{return_rast} is \code{TRUE} or
\code{outfile} is not specified, the resulting \code{RasterLayer} is
returned, otherwise \code{NULL} is returned invisibly.
}
\description{
Returns a raster with values only for the n cells with highest establishment
likelihood.
}
\details{
This function returns a raster containing values for only the top
\emph{n} cells.
}
\section{Warning}{
 This function uses the quantile approach for determining
the top \emph{n} cells. As such, if risk is highly aggregated this function may
run into issues when \code{n_cells} is high.
}

