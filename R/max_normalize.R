#' Raster max normalization
#'
#' Rescales raster values such that maximum is 1.
#'
#' @param rast Raster object.
#' @param outfile Character. Output raster file path. If not specified, the
#'   resulting `RasterLayer` will be  returned to R. Directory will be
#'   created recursively if required.
#' @return A normalised `RasterLayer` will be written to `outfile`
#'   if specified, and returned to R otherwise.
#' @importFrom raster raster setMinMax maxValue setValues getValues writeRaster
#' @export
max_normalize <- function(rast, outfile) {

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

  # Unit normalization
  out <- raster::setValues(r, (raster::getValues(r))/(r_max))

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
