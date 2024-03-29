% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/download_worldclim2.R
\name{download_worldclim2}
\alias{download_worldclim2}
\title{Download climate layers from WorldClim 2.0}
\usage{
download_worldclim2(outfile, variable, resolution)
}
\arguments{
\item{outfile}{Character. Target file for downloaded .zip archive.}

\item{variable}{Character. Can be one of \code{"bio"} (19 standard bioclim
variables), \code{"tmin"}, \code{"tmax"}, \code{"tavg"}, \code{"srad"},
\code{"wind"} or \code{"vapr"}.}

\item{resolution}{Character. Can be one of \code{"10m"}, \code{"5m"},
\code{"2.5m"}, or \code{"30s"}.}
}
\value{
A zipfile is downloaded and \code{NULL} is returned invisibly.
}
\description{
Download climate layers from WorldClim 2.0 (current climate only).
}
\examples{
\dontrun{
download_worldclim2('bioclim_10m', 'bio', '10m')
}
}
\seealso{
\code{\link[=extract_worldclim2]{extract_worldclim2()}}
}
