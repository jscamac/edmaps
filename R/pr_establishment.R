#' Calculate probability of establishment
#'
#' Calculate probability of establishment for each raster cell as a function of
#' probabilities of arrival and climate and/or biotic suitability.

#' @param rast `RasterLayer`, single-layer [`SpatRaster`], or path to a raster
#'   file containing pathway probability of pest arrival.
#' @param suitability `RasterLayer`, single-layer [`SpatRaster`], or path to a
#'   raster file containing a suitability raster. Cell values must be between 0
#'   (not suitable) and 1 (ideal suitability).
#' @param outfile Character. Output raster file path. If not provided, the
#'   [`SpatRaster`] will be returned to R.
#' @param return_rast Logical. Should the [`SpatRaster`] be returned to R?
#'   Ignored if `outfile` is not provided.
#' @return If `outfile` is specified, the resulting [`SpatRaster`] is saved to
#'   `outfile`. If `return_rast` is `TRUE` or `outfile` is not specified, the
#'   resulting [`SpatRaster`] is returned, otherwise `NULL` is returned
#'   invisibly.
#' @importFrom terra rast minmax setMinMax writeRaster compareGeom
#' @importFrom methods is
#' @export
pr_establishment <- function(rast, suitability, outfile, return_rast) {

  if(is.character(rast) || is(rast, 'RasterLayer')) {
    rast <- terra::rast(rast)
  } else if(!is(rast, 'SpatRaster') || dim(rast)[3] > 1) {
    stop('rast must be a RasterLayer, single-layer SpatRaster, ',
         'or a single path to a raster file.', call.=FALSE)
  }
  terra::setMinMax(rast)

  if(is.character(suitability) || is(suitability, 'RasterLayer')) {
    suitability <- terra::rast(suitability)
  } else if(!is(suitability, 'SpatRaster') || dim(suitability)[3] > 1) {
    stop('suitability must be a RasterLayer, single-layer SpatRaster, ',
         'or a single path to a raster file.', call.=FALSE)
  }
  terra::setMinMax(suitability)

  rng <- terra::minmax(suitability)
  if(rng[1] < 0 || rng[2] > 1) {
    stop("Suitability raster scores should be between zero (not suitable) and ",
         "1 (ideal suitability)", call.=FALSE)
  }
  terra::compareGeom(rast, suitability, crs=FALSE)
  out <- rast * suitability

  if(!missing(outfile)) {
    # Create directory if it does not exist
    if(!dir.exists(dirname(outfile))) {
      dir.create(dirname(outfile), recursive = TRUE)
    }
    # write out raster
    terra::writeRaster(out, outfile, overwrite = TRUE)
  }
  if(isTRUE(return_rast) || missing(outfile)) {
    out
  } else {
    invisible(NULL)
  }
}
