#' Estimates pest arrivals due to Torres Strait air passengers
#'
#' Estimates pest arrivals due to Torres Strait air passengers coming into
#' Cairns.
#'
#' @param pop_density A \code{RasterLayer} or file path to a raster file 
#'   containing population density.
#' @param airport_weight A \code{RasterLayer} or file path to a raster file 
#'   containing distance from Cairns airport weights.
#' @param total_passengers Integer. The number of passengers arriving to Cairns
#'   from Torres Strait.
#' @param probability Numeric vector of one or more probabilities that a Torres
#'   Strait passenger carries a pest.
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
arrivals_by_torres <- function(pop_density, airport_weight, total_passengers,
  probability, outfile, summarise_uncertainty=FALSE, return_rast=FALSE) {

  #Load raster
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
  out <- disperse_arrivals(r, total_passengers, probability[1])
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
    raster::writeRaster(out, outfile, overwrite = TRUE)
  }
  if(isTRUE(return_rast) || missing(outfile)) {
    out
  } else {
    invisible(NULL)
  }
}
