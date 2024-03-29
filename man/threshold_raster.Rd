% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/threshold_raster.R
\name{threshold_raster}
\alias{threshold_raster}
\title{Threshold raster}
\usage{
threshold_raster(rast, threshold, value = 0, outfile)
}
\arguments{
\item{rast}{A file path to a raster file.}

\item{threshold}{A named list giving the minimum and/or maximum values
defining the range of values to retain. Values outside this range will be
replaced with \code{value}. Can be \code{list(min = Y, max = Z)} or
\code{list(min = Y)} or \code{list(max = Z)}.}

\item{value}{Numeric. The value supplied to cells beyond the threshold(s).}

\item{outfile}{Character. Output raster file path. Parent directory will be
created recursively if required. If \code{outfile} is not provided, the
resulting \code{RasterLayer} will be returned to R.}
}
\value{
A \code{RasterLayer} will be written to \code{outfile} if provided,
and returned to R otherwise.
}
\description{
Threshold raster.
}
