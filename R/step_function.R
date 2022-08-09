#' Apply a step function to raster values
#'
#' Reclassify raster values either side of a defined threshold value.
#'
#' @param rast A `Raster*` or [`SpatRaster`] object.
#' @param threshold Numeric. The threshold value.
#' @param lower_value Numeric. The value to use below the threshold.
#' @param upper_value Numeric. The value to use above the threshold.
#' @return A [`SpatRaster`] object.
#' @importFrom terra setValues values
#' @export
step_function <- function(rast, threshold, lower_value, upper_value) {
	terra::setValues(rast, ifelse(terra::values(rast) >
		threshold, upper_value, lower_value))
}
