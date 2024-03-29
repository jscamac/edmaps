% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/port_weights.R
\name{port_weights}
\alias{port_weights}
\title{Create raster giving the establishment likelihoods relating to ports.}
\usage{
port_weights(template_raster, port_data, beta, outfile, return_rast = FALSE)
}
\arguments{
\item{template_raster}{A \code{RasterLayer} or path to supported raster
file.}

\item{port_data}{Character. Path to csv file containing port container
volumes and named Latitude and Longitude (in WGS84) for each port of
interest.}

\item{beta}{Numeric. The beta coefficient exponential function of how risk
changes with distance from port. beta should be in units per 1km.}

\item{outfile}{Character. Output raster file path. Directory will be
created if it does not exist.If not provided, object will be returned to
R.}

\item{return_rast}{Logical. Should the resulting raster be returned to R?
Ignored if \code{outfile} is not provided.}
}
\value{
If \code{outfile} is specified, the resulting raster is saved as a
geotiff to that path. If \code{return_rast} is \code{TRUE} or
\code{outfile} is not specified the resulting raster is returned,
otherwise \code{NULL} is returned invisibly.
}
\description{
Creates spatial port establishment likelihood raster as a function of port
container volumes and distance from port.
}
