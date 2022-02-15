#' Transform airport distances
#'
#' Transform airport distances according to a negative exponential function.
#'
#' @param airport_dist Character. File path to a raster file containing
#'   proximity to airports. Map units are expected to be kilometres.
#' @param beta Numeric. Parameter passed to the exponential function. Distance
#'   to nearest airport is multiplied by this value and exponentiated to give
#'   the relative density of tourists at a location. To generate a distribution
#'   that ensures proportion _p_ of tourists within distance _d_ of
#'   nearest airport, specify `airport_beta=log(p)/d` (e.g. to have 50% of
#'   tourists within 200 km of an airport, use `log(0.5)/200`).
#' @param outfile Character. Output raster file path. If missing, object will
#'   be returned to R.
#' @param overwrite Logical. Overwite the target raster if it already exists?
#' @param return_rast Logical. Should the raster object be returned to R?
#' @return If `return_rast` is TRUE, or if `outfile` is missing,
#'   the resulting `RasterLayer` object will be returned. Otherwise
#'   `NULL` is returned invisibly.
#' @importFrom raster raster writeRaster
#' @export
weight_airport_dist <- function(airport_dist, beta = log(0.5)/200, outfile,
  overwrite = FALSE, return_rast = FALSE) {

  ad <- raster::raster(airport_dist)
  out <- exp_function(rast = ad, beta = beta)
  names(out) <- "airport_dist_weight"

  if(!missing(outfile)) {
    # Create directory if it does not exist
    if(!dir.exists(dirname(outfile))) {
      dir.create(dirname(outfile), recursive = TRUE)
    }
    # write out raster
    raster::writeRaster(out, outfile, overwrite = overwrite)
  }

  if(isTRUE(return_rast) || missing(outfile)) out else invisible(NULL)
}
