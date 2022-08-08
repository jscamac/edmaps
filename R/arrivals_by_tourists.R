#' Estimates pest arrival due to tourists
#'
#' Estimates pest arrival due to tourists as a function of distance from airport
#' and tourist accommodation.
#'
#' @param tourist_beds A [`SpatRaster`] or `RasterLayer` object, or a file path
#'   to a raster file containing tourist bed numbers.
#' @param airport_weights A `RasterLayer` or file path to a raster file
#'   containing airport distance weights.
#' @param leakage_rate Numeric vector of 2 values, giving the lower and upper
#'   bounds of a 95% CI for leakage rate (the number of pest leakage events in
#'   a random year).
#' @param establishment_rate Numeric vector of 2 values, giving the lower and
#'   upper bounds of a 95% CI for establishment rate (the rate of survival, to
#'   establishment, for leakage events).
#' @param outfile Character. Output raster file path. If `probability` has
#'   length > 1, the file type must support multiband rasters (e.g. GeoTiff). If
#'   not provided, [`SpatRaster`] object will be returned to R.
#' @param return_rast Logical. Should the [`SpatRaster`] object be returned to
#'   R? Ignored if `outfile` is not provided.
#' @param overwrite Logical. Should `outfile` be overwritten if it exists?
#'   Default is `FALSE`.
#' @return If `outfile` is specified, the resulting [`SpatRaster`] (multiband if
#'   `probability` has length > 1) is saved as a tiff at that path. If
#'   `return_rast` is `TRUE` or `outfile` is not specified the resulting
#'   [`SpatRaster`] object is returned, otherwise `NULL` is returned invisibly.
#' @family functions estimating arrivals
#' @importFrom terra rast writeRaster
#' @importFrom methods is
#' @export
arrivals_by_tourists <- function(tourist_beds, airport_weights,
  leakage_rate, establishment_rate, outfile, return_rast=FALSE,
  overwrite=FALSE) {

  if(length(leakage_rate) != 2) {
    stop('leakage_rate must be a vector of two values.')
  }
  if(length(establishment_rate) != 2) {
    stop('establishment_rate must be a vector of two values.')
  }

  # Load rasters
  if(is.character(tourist_beds) || is(tourist_beds, 'Raster')) {
    tourist_beds <- terra::rast(tourist_beds)
  } else if(!is(tourist_beds, 'SpatRaster')) {
    stop('tourist_beds must be a SpatRaster or RasterLayer object, ',
         'or a file path to a raster file.')
  }
  if(is.character(airport_weights) || is(airport_weights, 'Raster')) {
    airport_weights <- terra::rast(airport_weights)
  } else if(!is(airport_weights, 'SpatRaster')) {
    stop('airport_weights must be a SpatRaster or RasterLayer object, ',
         'or a file path to a raster file.')
  }

  # Multiply rasters
  r <- tourist_beds * airport_weights

  # Convert weighted values to national proportion
  r <- calc_proportion(r)

  # Disperse passengers and calculate arrival rate
  EE <- calc_EE(leakage_rate, establishment_rate)
  out <- calc_pathway_pr(EE, r, return_rast=TRUE)

  if(!missing(outfile)) {
    # Create directory if it does not exist
    if(!dir.exists(dirname(outfile))) {
      dir.create(dirname(outfile), recursive = TRUE)
    }
    # write out raster
    terra::writeRaster(out, outfile, overwrite = overwrite)
  }
  if(isTRUE(return_rast) || missing(outfile)) {
    out
  } else {
    invisible(NULL)
  }
}
