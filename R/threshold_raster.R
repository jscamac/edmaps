#' Threshold raster
#'
#' Threshold raster.
#'
#' @param rast `Raster*`, [`SpatRaster`], or a character vector of one or more
#'   path to raster files.
#' @param threshold A named list giving the minimum and/or maximum values
#'   defining the range of values to retain. Values outside this range will be
#'   replaced with `value`. Can be `list(min = Y, max = Z)` or `list(min = Y)`
#'   or `list(max = Z)`.
#' @param value Numeric. The value supplied to cells beyond the threshold(s).
#' @param outfile Character. Output raster file path. Parent directory will be
#'   created recursively if required. If `outfile` is not provided, the
#'   resulting [`SpatRaster`] will be returned to R.
#' @return A [`SpatRaster`] will be written to `outfile` if provided, and
#'   returned to R otherwise.
#' @importFrom terra rast writeRaster
#' @export
threshold_raster <- function(rast, threshold, value = 0, outfile) {

  if(is.character(rast) || is(rast, 'Raster')) {
    rast <- terra::rast(rast)
  } else if(!is(rast, 'SpatRaster')) {
    stop('rast must be a Raster* or SpatRaster object, or a character vector ',
         'of one or more paths to raster files.')
  }

  if(length(threshold)==2 && all(c("min", "max") %in% names(threshold))) {
    out[out < threshold[['min']] | out > threshold[['max']]]  <- value
  } else if(length(threshold)==1 && names(threshold) == "min") {
    out[out < threshold[['min']]]  <- value
  } else if(length(threshold)==1 && names(threshold) =="max") {
    out[out > threshold[['max']]]  <- value
  } else {
    stop("threshold list can only contain elements named 'min' and/or 'max'.")
  }
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
