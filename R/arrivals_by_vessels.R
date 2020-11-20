#' Estimates pest arrivals by vessels
#'
#' Estimates pest arrivals by vessels.
#'
#' @param port_weight A \code{RasterLayer} or file path to a raster file 
#'   containing weights to be used to distribute arrivals.
#' @param n_vessels Integer. The total number of vessels entering country.
#' @param probability Numeric vector of one or more probabilities that a unit
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
#'
arrivals_by_vessels <- function(port_weight, n_vessels, probability, outfile, 
  summarise_uncertainty=FALSE, return_rast=FALSE) {

  if(is.character(port_weight)) {
    port_weight <- raster::raster(port_weight)
  }

  # Disperse passengers and calculate arrival rate
  out <- disperse_arrivals(port_weight, n_vessels, probability[1])
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
