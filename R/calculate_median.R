#' Calculate median of raster objects
#'
#' Calculate the cellwise median of multiple rasters.
#'
#' @param x Either a `RasterStack`, a multi-layer  [`SpatRaster`], or a vector
#'   of file paths to raster files.
#' @param outfile Character. Optional output raster file path. Directory will be
#'   created recursively if it does not exist.
#' @return If `outfile` is specified, the resulting [`SpatRaster`] is saved to
#'   that path and `outfile` is returned invisibly. If `outfile` is not
#'   specified the [`SpatRaster`] is returned to R.
#' @importFrom terra rast median writeRaster
#' @importFrom methods is
#' @export
calculate_median <- function(x, outfile) {
  if(is.character(x) || is(x, 'RasterStack')) {
    x <- terra::rast(x)
  } else if(!is(x, 'SpatRaster')) {
    stop('x must be a RasterStack, SpatRaster, or vector of raster file paths.')
  }
  result <- terra::median(x)
  if(!dir.exists(dirname(outfile))) dir.create(dirname(outfile), recursive=TRUE)
  if(!missing(outfile)) {
    terra::writeRaster(result, outfile)
    return(outfile)
  } else {
    result
  }
}
