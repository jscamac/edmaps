#' Estimates pest arrivals via wind
#'
#' Estimates pest arrivals via coastal onshore winds.
#'
#' @param wind_speed A `RasterLayer` or file path to a raster file
#'   containing wind speed.
#' @param leakage_rate Numeric vector of 2 values, giving the lower and upper
#'   bounds of a 95% CI for leakage rate (the number of pest leakage events in
#'   a random year).
#' @param establishment_rate Numeric vector of 2 values, giving the lower and
#'   upper bounds of a 95% CI for establishment rate (the rate of survival, to
#'   establishment, for leakage events).
#' @param outfile Character. Output raster file path. If not provided, raster
#'   object will be returned to R.
#' @param return_rast Logical. Should the raster object be returned to R?
#'   Ignored if `outfile` is not provided.
#' @param overwrite Logical. If `TRUE` and `outfile` is not missing,
#'   it will be overwritten if the file specified by `outfile` already
#'   exists.
#' @return If `outfile` is specified, the resulting raster (multiband if
#'   `probability` has length > 1) is saved as a tiff at that path. If
#'   `return_rast` is `TRUE` or `outfile` is not specified the
#'   resulting raster object is returned, otherwise `NULL` is returned
#'   invisibly.
#' @family functions estimating arrivals
#' @importFrom raster raster writeRaster stack
#' @export
arrivals_by_wind <- function(wind_speed, leakage_rate, establishment_rate,
  outfile, return_rast=FALSE, overwrite=FALSE) {

  if(length(leakage_rate) != 2) {
    stop('leakage_rate must be a vector of two values.')
  }
  if(length(establishment_rate) != 2) {
    stop('establishment_rate must be a vector of two values.')
  }

  # Load raster
  if(is.character(wind_speed)) {
    wind <- raster::raster(wind_speed)
  } else if(is(wind_speed, 'RasterLayer')) {
    wind <- wind_speed
  } else {
    stop('wind_speed must be a RasterLayer or a file path to a raster file.')
  }
  # Calculate proportion of population density
  prop_wind <- calc_proportion(wind)

  # Disperse and calculate arrival rate
  EE <- calc_EE(leakage_rate, establishment_rate)
  out <- calc_pathway_pr(EE, prop_wind, return_rast=TRUE)

  if(!missing(outfile)) {
    # Create directory if it does not exist
    if(!dir.exists(dirname(outfile))) {
      dir.create(dirname(outfile), recursive = TRUE)
    }
    # write out raster
    raster::writeRaster(out, outfile, overwrite = overwrite)
  }
  if(isTRUE(return_rast) || missing(outfile)) {
    out
  } else {
    invisible(NULL)
  }
}
