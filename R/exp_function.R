#' Exponentiate raster values
#'
#' Apply exponential function to raster values.
#'
#' @param rast [`SpatRaster`] object.
#' @param beta Numeric. The beta coefficient of a standard exponential function.
#' @return A [`SpatRaster`] object.
#' @importFrom terra setValues getValues
#' @export
exp_function <- function(rast, beta) {
  terra::setValues(rast, exp(beta * terra::values(rast)))
}
