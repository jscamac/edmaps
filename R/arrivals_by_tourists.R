#' Estimates pest arrival due to tourists
#'
#' Estimates pest arrival due to tourists as a function of distance from airport
#' and tourist accommodation.
#'
#' @param tourist_beds A \code{RasterLayer} or file path to a raster file 
#'   containing tourist bed numbers.
#' @param airport_weights A \code{RasterLayer} or file path to a raster file 
#'   containing airport distance weights.
#' @param total_tourists Integer. The number of tourists.
#' @param probability Numeric. The probability a tourist carries a pest.
#' @param outfile Character. Output raster file path. If not provided, raster 
#'   object will be returned to R.
#' @param return_rast Logical. Should the raster object be returned to R?
#'   Ignored if \code{outfile} is not provided.
#' @return If \code{outfile} is specified, the resulting raster is saved as a
#'   to that path. If \code{return_rast} is \code{TRUE} or \code{outfile} is 
#'   not specified the resulting raster is returned, otherwise \code{NULL} is 
#'   returned invisibly.
#' @family functions estimating arrivals
#' @importFrom raster raster writeRaster
#' @export
arrivals_by_tourists <- function(tourist_beds, airport_weights, 
  total_tourists, probability, outfile, return_rast=FALSE) {

  #Load rasters
  tb <- raster::raster(tourist_beds)
  ad_weight <- raster::raster(airport_weights)

  # Multiply rasters
  r <- tb * ad_weight

  # Convert weighted values to national proportion
  r <- calc_proportion(r)

  # Disperse passengers and calculate arrival rate
  out <- disperse_arrivals(r, total_tourists, probability)
  names(out) <- c("arrivals_by_tourists")

  if(!missing(outfile)) {
    # Create directory if it does not exist
    if(!dir.exists(dirname(outfile))) {
      dir.create(dirname(outfile), recursive = TRUE)
    }
    # write out raster
    raster::writeRaster(out, outfile, overwrite=TRUE)
  }
  if(isTRUE(return_rast) || missing(outfile)) {
    out
  } else {
    invisible(NULL)
  }
}
