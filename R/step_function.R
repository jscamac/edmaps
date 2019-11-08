#' Apply a step function to raster values
#'
#' Reclassify raster values either side of a defined threshold value.
#' 
#' @param rast Raster object
#' @param threshold Numeric. The threshold value.
#' @param lower_value Numeric. The value to set below the threshold.
#' @param upper_value Numeric. The value to set above the threshold.
#' @return A raster object
#' @importFrom raster setValues getValues
#' @export
step_function <- function(rast, threshold, lower_value, upper_value) {
	raster::setValues(rast, ifelse(raster::getValues(rast) >
		threshold, upper_value, lower_value))
}
