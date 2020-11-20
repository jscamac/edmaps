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
#' @param probability Numeric vector of one or more probabilities that a tourist
#'   carries a pest.
#' @param outfile Character. Output raster file path. If \code{probability} has
#'   length > 1, the file type must support multiband rasters (e.g. GeoTiff). If
#'   not provided, raster object will be returned to R.
#' @param return_rast Logical. Should the raster object be returned to R?
#'   Ignored if \code{outfile} is not provided.
#' @return If \code{outfile} is specified, the resulting raster (multiband if
#'   \code{probability} has length > 1) is saved as a tiff at that path. If
#'   \code{return_rast} is \code{TRUE} or \code{outfile} is not specified the
#'   resulting raster object is returned, otherwise \code{NULL} is returned 
#'   invisibly.
#' @family functions estimating arrivals
#' @importFrom raster raster writeRaster stack
#' @export
arrivals_by_tourists <- function(tourist_beds, airport_weights, 
  total_tourists, probability, outfile, summarise_uncertainty=FALSE, 
  return_rast=FALSE) {

  #Load rasters
  tb <- raster::raster(tourist_beds)
  ad_weight <- raster::raster(airport_weights)

  # Multiply rasters
  r <- tb * ad_weight

  # Convert weighted values to national proportion
  r <- calc_proportion(r)

  # Disperse passengers and calculate arrival rate
  out <- disperse_arrivals(r, total_tourists, probability[1])
  if(length(probability > 1)) {
    out <- raster::stack(
      c(list(out), lapply(probability[-1]/probability[1], function(x) {
        out*x
      }))
    )
  }

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
