#' Raster max normalization
#'
#' Rescales raster values such that maximum is 1.
#'
#' @param rast Raster*, [`SpatRaster`], or character vector of one or more paths
#'   to raster files.
#' @param outfile Character. Output raster file path. If not specified, the
#'   resulting [`SpatRaster`] will be  returned to R. Directory will be created
#'   recursively if required.
#' @return A normalised [`SpatRaster`] will be written to `outfile` if
#'   specified, and returned to R otherwise.
#' @importFrom terra rast setMinMax minmax setValues values writeRaster
#' @export
max_normalize <- function(rast, outfile) {

  if(is.character(rast) || is(rast, 'Raster')) {
    rast <- terra::rast(rast)
  } else if(!is(rast, 'SpatRaster')) {
    stop('rast must be a Raster*, SpatRaster, or a character vector giving ',
         'one or more file path to raster files.')
  }

  # Ensure min and max is calculated
  terra::setMinMax(rast)

  # Calculate max value
  r_max <- terra::minmax(rast)[2, ]

  # Unit normalization
  out <- terra::setValues(rast, sweep(terra::values(rast), 2, r_max, `/`))

  if(!missing(outfile)) {
    # Create directory if it does not exist
    if(!dir.exists(dirname(outfile))) {
      dir.create(dirname(outfile), recursive = TRUE)
    }
    # write out raster
    terra::writeRaster(out, outfile, overwrite=TRUE)
  } else {
    out
  }
}
