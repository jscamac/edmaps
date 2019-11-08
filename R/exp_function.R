#' Exponentiate raster values
#'
#' Apply exponential function to raster values.
#' 
#' @param rast Raster object.
#' @param beta Numeric. The beta coefficient of a standard exponential function.
#' @return A Raster object.
#' @importFrom raster setValues getValues
#' @export
exp_function <- function(rast, beta) {
  raster::setValues(rast, exp(beta * raster::getValues(rast)))
}
