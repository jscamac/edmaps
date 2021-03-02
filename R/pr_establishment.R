#' Calculate probability of establishment
#'
#' Calculate probability of establishment for each raster cell as a function of
#' probabilities of arrival and climate and/or biotic suitability.
#' @param rast Raster object or path to file containing pathway probability of
#'   pest arrival.
#' @param suitability Raster object or path to file containing a suitability
#'   raster. Cell values must be between 0 (not suitable) and 1 (ideal
#'   suitability).
#' @param outfile Character. Output raster file path. If not provided, the
#' \code{RasterLayer} will be returned to R.
#' @param return_rast Logical. Should the \code{RasterLayer} be returned to R?
#'   Ignored if \code{outfile} is not provided.
#' @return If \code{outfile} is specified, the resulting \code{RasterLayer} is
#'   saved to \code{outfile}. If \code{return_rast} is \code{TRUE} or
#'   \code{outfile} is not specified, the resulting \code{RasterLayer} is
#'   returned, otherwise \code{NULL} is returned invisibly.
#' @importFrom raster raster stack minValue maxValue writeRaster
#' @export
#' @export

pr_establishment <- function(rast, suitability, outfile, return_rast) {

  if(is.character(rast)) {
    rast <- raster::raster(rast)
  }

  if(is.character(suitability)) {
    suitability <- raster::raster(suitability)
  }

  if(raster::minValue(suitability) < 0 || raster::maxValue(suitability) > 1) {
    stop("Suitability raster scores should be between zero (not suitable) and ",
         "1 (ideal suitability)")
  }
  raster::compareRaster(rast, suitability, crs=FALSE)
  out <- rast * suitability

  if(!missing(outfile)) {
    # Create directory if it does not exist
    if(!dir.exists(dirname(outfile))) {
      dir.create(dirname(outfile), recursive = TRUE)
    }
    # write out raster
    raster::writeRaster(out, outfile, overwrite = TRUE)
  }
  if(isTRUE(return_rast) || missing(outfile)) {
    out
  } else {
    invisible(NULL)
  }
}
