#' Estimates pest arrivals
#'
#' Estimates pest arrivals by containers.
#'
#' @param weight A [`SpatRaster`] or [`RasterLayer`] object, or a file path to a
#'   raster dataset in a format supported by OGR, as produced by
#'   [container_weights()].
#' @param leakage Numeric vector of 2 values, giving the lower and upper bounds
#'   of a 95% CI for leakage rate (the number of pest leakage events in a random
#'   year).
#' @param viability Numeric vector of 2 values, giving the lower and upper
#'   bounds of a 95% CI for viability rate (the rate of survival, to
#'   establishment, for leakage events).
#' @param outfile Character. Output raster file path. If `probability` has
#'   length > 1, the file type must support multiband rasters (e.g. GeoTiff). If
#'   not provided, the resulting [`SpatRaster`] object will be returned to R.
#' @param return_rast Logical. Should the [`SpatRaster`] object be returned to
#'   R? Ignored if `outfile` is not provided.
#' @param overwrite Logical. Should `outfile` be overwritten if it exists?
#'   Default is `FALSE`.
#' @return If `outfile` is specified, the resulting [`SpatRaster`] object is
#'   saved as a GeoTiff at that path. If `return_rast` is `TRUE` or `outfile` is
#'   not specified, the resulting [`SpatRaster`] object is returned, otherwise
#'   `NULL` is returned invisibly.
#' @family functions estimating arrivals
#' @importFrom terra rast writeRaster
#' @export
arrivals <- function(weight, leakage, viability, outfile, return_rast=FALSE,
                     overwrite=FALSE) {

  if(length(leakage) != 2 || diff(leakage) <= 0)
    stop('`leakage` must be a vector of length 2, giving the lower and ',
         'upper bounds of the 95% CI for leakage rate.', call.=FALSE)
  if(length(viability) != 2 || diff(viability) <= 0)
    stop('`viability` must be a vector of length 2, giving the lower and ',
         'upper bounds of the 95% CI for viability rate.', call.=FALSE)

  if(is.character(weight) || is(weight, 'Raster')) {
    weight <- terra::rast(weight)
  } else if(!is(weight, 'SpatRaster')) {
    stop('`weight` must be a SpatRaster or RasterLayer object, ',
         'or a file path to a raster file.', call.=FALSE)
  }

  weight <- calc_proportion(weight)
  establishment <- calc_establishment(leakage, viability)
  out <- calc_pathway_pr(establishment, weight, return_rast=TRUE)

  if(!missing(outfile)) {
    if(!dir.exists(dirname(outfile))) {
      dir.create(dirname(outfile), recursive=TRUE)
    }

    terra::writeRaster(out, outfile, overwrite=overwrite)
  }
  if(isTRUE(return_rast) || missing(outfile)) {
    out
  } else {
    invisible(NULL)
  }
}
