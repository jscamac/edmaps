#' Calculate raster cell values as proportions
#'
#' Calculates raster cell values as a proportion of the sum of all cells' values.
#'
#' @param rast Raster object.
#' @return A `RasterLayer`.
#' @importFrom raster getValues setValues
#' @export
calc_proportion <- function(rast) {
  total <- sum(raster::getValues(rast), na.rm=TRUE)
  raster::setValues(rast, (raster::getValues(rast)/total))
}
