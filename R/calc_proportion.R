#' Calculate raster cell values as proportions
#'
#' Calculates raster cell values as a proportion of the sum of all cells' values.
#'
#' @param rast A [`SpatRaster`] or `Raster*` object.
#' @return A [`SpatRaster`] object.
#' @importFrom terra rast global
#' @importFrom methods is
#' @export
calc_proportion <- function(rast) {
  if(is(rast, 'RasterLayer')) rast <- terra::rast(rast)
  if(!is(rast, 'SpatRaster') || dim(rast)[3] > 1) {
    stop('rast must be a RasterLayer object, or a SpatRaster object with a ',
         'single layer.')
  }
  total <- terra::global(rast, sum, na.rm=TRUE)$sum
  rast/total
}
