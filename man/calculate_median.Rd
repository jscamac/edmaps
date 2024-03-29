% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculate_median.R
\name{calculate_median}
\alias{calculate_median}
\title{Calculate median of raster objects}
\usage{
calculate_median(files, outfile)
}
\arguments{
\item{files}{A vector of file paths to (optionally multiband) raster files.}

\item{outfile}{Character. Output raster file path. If not provided, object
will be returned to R. Directory will be created recursively if it does
not exist.}
}
\value{
If \code{outfile} is specified, the resulting raster is saved as a
geotiff to that path and NULL is returned. If \code{outfile} is not
specified the resulting raster is returned.
}
\description{
Calculate the cellwise median of two or more Raster* objects, by layer.
}
