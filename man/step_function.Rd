% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/step_function.R
\name{step_function}
\alias{step_function}
\title{Apply a step function to raster values}
\usage{
step_function(rast, threshold, lower_value, upper_value)
}
\arguments{
\item{rast}{Raster object}

\item{threshold}{Numeric. The threshold value.}

\item{lower_value}{Numeric. The value to set below the threshold.}

\item{upper_value}{Numeric. The value to set above the threshold.}
}
\value{
A raster object
}
\description{
Reclassify raster values either side of a defined threshold value.
}
