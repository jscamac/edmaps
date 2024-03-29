% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/range_bag.R
\name{range_bag}
\alias{range_bag}
\title{Fit and project range bag model}
\usage{
range_bag(
  occurrence_data,
  bioclim_dir,
  n_dims = 2,
  n_models = 100,
  p = 0.5,
  exclude_vars = NULL,
  outfile
)
}
\arguments{
\item{occurrence_data}{\code{sf} object, \code{data.frame} or character
path to a csv file containing occurrence coordinates (must contain
columns named "Latitude" and "Longitude").}

\item{bioclim_dir}{Path. Path to directory containing WorldClim raster data.}

\item{n_dims}{Integer. The number of dimensions ranges to bag.}

\item{n_models}{Integer. The number of bootstrapped model ensembles to run.}

\item{p}{Numeric between 0 and 1. The proportion of occurrence records to
include in bootstrapping .}

\item{exclude_vars}{Character vector. A vector of bioclim variables to
exclude from analysis. Default is \code{NULL}.}

\item{outfile}{Character. Output raster file path. Parent directory will be
created recursively if required. If \code{NULL}, the \code{RasterLayer}
will be returned in R.}
}
\value{
A \code{RasterLayer} of model predictions is written to
\code{outfile} if provided, and returned to R otherwise. The raster's
extent, resolution and CRS are taken from the raster data in
\code{bioclim_dir}. Cell values give the fraction of bootstrapped models
for which the cell's environment fell within the species' modelled
climate envelope.
}
\description{
Fit and project range bag model.
}
\references{
This function is a modified version of the \code{rb} function
provided in Drake, J.M. & Richards, R.L. (2019)
\href{https://doi.org/10.5061/dryad.g5p7d1c]}{Data from: Estimating environmental suitability.} Dryad, Dataset, doi:10.5061/dryad.g5p7d1c.

See also: Drake, J.M. (2015)
\href{https://doi.org/10.1098/rsif.2015.0086}{Range bagging: a new method for ecological niche modelling from presence-only data.}
\emph{Journal of the Royal Society Interface}, 12(107), 20150086.
doi:https://doi.org/10.1098/rsif.2015.0086.
}
