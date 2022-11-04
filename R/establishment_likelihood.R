#' Estimate establishment likelihood
#'
#' Estimate the likelihood of pest establishment based on total arrivals and
#' environmental suitability.
#'
#' @param total_arrivals A `Raster*`object, [`SpatRaster`] object, or path to
#'   raster file(s) containing total arrival estimates.
#' @param suitability A `RasterLayer`, single-layer [`SpatRaster`], or path to
#'   raster file containing suitability scores.
#' @param outfile Character. Output raster file path. Must support multiband
#'   raster if `total_arrivals` has multiple layers. If not provided, the
#'   `Raster*` object will be returned to R.
#' @param return_rast Logical. Should the [`SpatRaster`] be returned to R?
#'   Ignored if `outfile` is not provided.
#' @return If `outfile` is specified, the resulting [`SpatRaster`] is saved as
#'   to that path. If `return_rast` is `TRUE` or `outfile` is not specified the
#'   resulting [`SpatRaster`] is returned, otherwise `outfile` is returned
#'   invisibly.
#' @importFrom terra rast writeRaster
#' @importFrom methods is
#' @export
establishment_likelihood <- function(total_arrivals, suitability, outfile,
  return_rast = FALSE) {

  if(is(suitability, 'RasterLayer') || is.character(suitability) &&
     length(suitability == 1)) {
    suitability <- terra::rast(suitability)
  } else if(!is(suitability, 'SpatRaster') || dim(suitability)[3] > 1) {
    stop('suitability must be a RasterLayer, single-layer SpatRaster, or ',
         'character vector to a single raster file.')
  }

  if(is(total_arrivals, 'Raster') || is.character(total_arrivals)) {
    total_arrivals <- terra::rast(total_arrivals)
  } else if(!is(total_arrivals, 'SpatRaster')) {
    stop('total_arrivals must be a RasterLayer, SpatRaster, or ',
         'character vector of raster file path(s).')
  }

  out <- total_arrivals * suitability

  if(!missing(outfile)) {
    # Create directory if it does not exist
    if(!dir.exists(dirname(outfile))) {
      dir.create(dirname(outfile), recursive = TRUE)
    }
    # write out raster
    terra::writeRaster(out, outfile, overwrite=TRUE)
  }
  if(isTRUE(return_rast) || missing(outfile)) {
    out
  } else {
    invisible(outfile)
  }
}
