% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/arrivals_by_torres.R
\name{arrivals_by_torres}
\alias{arrivals_by_torres}
\title{Estimates pest arrivals due to Torres Strait air passengers}
\usage{
arrivals_by_torres(
  pop_density,
  airport_weight,
  total_passengers,
  probability,
  outfile,
  summarise_uncertainty = FALSE,
  return_rast = FALSE
)
}
\arguments{
\item{pop_density}{A \code{RasterLayer} or file path to a raster file 
containing population density.}

\item{airport_weight}{A \code{RasterLayer} or file path to a raster file 
containing distance from Cairns airport weights.}

\item{total_passengers}{Integer. The number of passengers arriving to Cairns
from Torres Strait.}

\item{probability}{Numeric vector of one or more probabilities that a Torres
Strait passenger carries a pest.}

\item{outfile}{Character. Output raster file path. If \code{probability} has
length > 1, the file type must support multiband rasters (e.g. GeoTiff). If
not provided, raster object will be returned to R.}

\item{return_rast}{Logical. Should the raster object be returned to R?
Ignored if \code{outfile} is not provided.}
}
\value{
If \code{outfile} is specified, the resulting raster (multiband if
  \code{probability} has length > 1) is saved as a tiff at that path. If
  \code{return_rast} is \code{TRUE} or \code{outfile} is not specified the
  resulting raster object is returned, otherwise \code{NULL} is returned 
  invisibly.
}
\description{
Estimates pest arrivals due to Torres Strait air passengers coming into
Cairns.
}
\seealso{
Other functions estimating arrivals: 
\code{\link{arrivals_by_containers}()},
\code{\link{arrivals_by_fertiliser}()},
\code{\link{arrivals_by_food}()},
\code{\link{arrivals_by_goods}()},
\code{\link{arrivals_by_machinery}()},
\code{\link{arrivals_by_mail}()},
\code{\link{arrivals_by_nurserystock}()},
\code{\link{arrivals_by_residents}()},
\code{\link{arrivals_by_tourists}()},
\code{\link{arrivals_by_vessels}()}
}
\concept{functions estimating arrivals}
