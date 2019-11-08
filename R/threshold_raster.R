#' Threshold raster
#'
#' Threshold raster.
#' 
#' @param rast A file path to a raster file.
#' @param threshold A named list giving the minimum and/or maximum values 
#'   defining the range of values to retain. Values outside this range will be 
#'   replaced with \code{value}. Can be \code{list(min = Y, max = Z)} or 
#'   \code{list(min = Y)} or \code{list(max = Z)}.
#' @param value Numeric. The value supplied to cells beyond the threshold(s).
#' @param outfile Character. Output raster file path. Parent directory will be 
#'   created recursively if required. If \code{outfile} is not provided, the 
#'   resulting \code{RasterLayer} will be returned to R.
#' @return A \code{RasterLayer} will be written to \code{outfile} if provided, 
#'   and returned to R otherwise.
#' @importFrom raster raster writeRaster
#' @export
threshold_raster <- function(rast, threshold, value = 0, outfile) {
  # Load raster
  if(is.character(rast)) {
    out <- raster::raster(rast)
  } else {
    out <- rast
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
    raster::writeRaster(out, outfile, overwrite=TRUE)
  } else {
    out
  }
}
