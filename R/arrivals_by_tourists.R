#' Estimates pest arrival due to tourists
#'
#' Estimates pest arrival due to tourists as a function of distance from airport
#' and tourist accommodation.
#'
#' @param tourist_beds A \code{RasterLayer} or file path to a raster file
#'   containing tourist bed numbers.
#' @param airport_weights A \code{RasterLayer} or file path to a raster file
#'   containing airport distance weights.
#' @param leakage_rate Numeric vector of 2 values, giving the lower and upper
#'   bounds of a 95% CI for leakage rate (the number of pest leakage events in
#'   a random year).
#' @param establishment_rate Numeric vector of 2 values, giving the lower and
#'   upper bounds of a 95% CI for establishment rate (the rate of survival, to
#'   establishment, for leakage events).
#' @param outfile Character. Output raster file path. If \code{probability} has
#'   length > 1, the file type must support multiband rasters (e.g. GeoTiff). If
#'   not provided, raster object will be returned to R.
#' @param return_rast Logical. Should the raster object be returned to R?
#'   Ignored if \code{outfile} is not provided.
#' @param overwrite Logical. If \code{TRUE} and \code{outfile} is not missing,
#'   it will be overwritten if the file specified by \code{outfile} already
#'   exists.
#' @return If \code{outfile} is specified, the resulting raster (multiband if
#'   \code{probability} has length > 1) is saved as a tiff at that path. If
#'   \code{return_rast} is \code{TRUE} or \code{outfile} is not specified the
#'   resulting raster object is returned, otherwise \code{NULL} is returned
#'   invisibly.
#' @family functions estimating arrivals
#' @importFrom raster raster writeRaster stack
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
  tb <- raster::raster(tourist_beds)
  ad_weight <- raster::raster(airport_weights)

  # Multiply rasters
  r <- tb * ad_weight

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
    raster::writeRaster(out, outfile, overwrite = overwrite)
  }
  if(isTRUE(return_rast) || missing(outfile)) {
    out
  } else {
    invisible(NULL)
  }
}
