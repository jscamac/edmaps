% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rasterize_wind.R
\name{rasterize_wind}
\alias{rasterize_wind}
\title{Rasterize wind data}
\usage{
rasterize_wind(data, wind_column, template, width, outfile)
}
\arguments{
\item{data}{Character. A vector polygon dataset (e.g. Shapefile or
GeoPackage) describing onshore winds, or a file path to such a file.}

\item{wind_column}{Character. The column name for the column of the object
defined by \code{data} that contains wind speed data.}

\item{template}{A \code{RasterLayer} or \code{stars} object, or a character
file path to a file that can be read by GDAL, defining the extent and
resolution of analysis, and defining the coastline (edge of non-NA values).
If a \code{stars} object, the first attribute will be used. Must have a
valid coordinate system. CRS is assumed to be Australian Albers (3577).}

\item{width}{Numeric. The width of the coastal buffer (in metres), defining
how far inland the wind has an effect. E.g. if the pest is thought to be
carried up to 50km inland by the wind, set this value to \code{50000}.}

\item{outfile}{Character. The target file path for the wind raster.}
}
\description{
Process and rasterize wind data, creating raster datasets that describe wind
speed within a specified distance of the coastline.
}
