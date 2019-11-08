#' Estimates pest arrivals by fertiliser imports
#'
#' Estimates pest arrivals by fertiliser imports.
#'
#' @param fertiliser_weight A \code{RasterLayer} or file path to a raster file 
#'   containing weights to distribute arrivals by.
#' @param fertiliser_units Integer. The total number of fertiliser units
#'   entering country.
#' @param probability Numeric. The probability a unit carries pest.
#' @param outfile Character. Output raster file path. If not provided, raster 
#'   object will be returned to R.
#' @param return_rast Logical. Should the raster object be returned to R?
#'   Ignored if \code{outfile} is not provided.
#' @return If \code{outfile} is specified, the resulting raster is saved as a
#'   to that path. If \code{return_rast} is \code{TRUE} or \code{outfile} is 
#'   not specified the resulting raster is returned, otherwise \code{NULL} is 
#'   returned invisibly.
#' @family functions estimating arrivals
#' @importFrom raster raster stack writeRaster
#' @export
#'
arrivals_by_fertiliser <- function(fertiliser_weight, fertiliser_units, 
  probability, outfile, return_rast=FALSE) {

  if(is.character(fertiliser_weight)) {
    fertiliser_weight <- raster::raster(fertiliser_weight)
  } else if(!is(fertiliser_weight, 'RasterLayer')) {
    stop('fertiliser_weight must be a RasterLayer or a file path to a raster file.')
  }

  # Disperse passengers and calculate arrival rate
  out <- disperse_arrivals(fertiliser_weight, fertiliser_units, probability)
  names(out) <- "arrivals_by_fertiliser"

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
