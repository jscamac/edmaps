#' Estimates pest arrivals due to Torres Strait air passengers
#'
#' Estimates pest arrivals due to Torres Strait air passengers coming into
#' Cairns.
#'
#' @param pop_density A \code{RasterLayer} or file path to a raster file
#'   containing population density.
#' @param airport_weight A \code{RasterLayer} or file path to a raster file
#'   containing distance from Cairns airport weights.
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
arrivals_by_torres <- function(pop_density, airport_weight, leakage_rate,
  establishment_rate, outfile, return_rast=FALSE, overwrite=FALSE) {

  if(length(leakage_rate) != 2) {
    stop('leakage_rate must be a vector of two values.')
  }
  if(length(establishment_rate) != 2) {
    stop('establishment_rate must be a vector of two values.')
  }

  # Load raster
  if(is.character(pop_density)) {
    pop <- raster::raster(pop_density)
  } else if(is(pop_density, 'RasterLayer')) {
    pop <- pop_density
  } else {
    stop('pop_density must be a RasterLayer or a file path to a raster file.')
  }

  ad_weight <- raster::raster(airport_weight)

  # Multiply rasters
  r <- pop * ad_weight

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
