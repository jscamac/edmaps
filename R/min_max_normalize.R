#' Raster min-max normalization
#'
#' Performs a min-max normalization on a raster object.
#' 
#' @param rast Raster object.
#' @param outfile Character. Output raster file path. If not specified, the 
#'   resulting \code{RasterLayer} will be  returned to R. Directory will be 
#'   created recursively if required.
#' @return A normalised \code{RasterLayer} will be written to \code{outfile} 
#'   if specified, and returned to R otherwise.
#' @importFrom raster raster setMinMax maxValue minValue setValues getValues writeRaster
#' @export
min_max_normalize <- function(rast, outfile) {

  # Load raster
  if(is.character(rast)) {
    r <- raster::raster(rast)
  } else {
    r <- rast
  }

  # Ensure min and max is calculated
  r <- raster::setMinMax(r)
  # Calculate max value
  r_max <- raster::maxValue(r)
  # Calculate min value
  r_min <- raster::minValue(r)

  # Unit normalization
  out <- raster::setValues(r, (raster::getValues(r) - r_min) / (r_max - r_min))

  if(!missing(outfile)) {
    # Create directory if it does not exist
    if(!dir.exists(dirname(outfile))) {
      dir.create(dirname(outfile), recursive = TRUE)
    }
    # write out raster
    raster::writeRaster(out, outfile, overwrite=TRUE)
  } else {
    out
  }
}
