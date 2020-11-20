#' Estimate establishment likelihood
#'
#' Estimate the likelihood of pest establishment based on total arrivals and
#' environmental suitability.
#'
#' @param total_arrivals A \code{Raster*} object or path to raster file
#'   containing total arrival estimates.
#' @param suitability A \code{RasterLayer} or path to raster file containing
#'   suitability scores.
#' @param outfile Character. Output raster file path. Must support multiband 
#'   raster if \code{total_arrivals} has multiple layers. If not provided, the
#'   \code{Raster*} object will be returned to R.
#' @param return_rast Logical. Should the \code{RasterLayer} be returned to R?
#'   Ignored if \code{outfile} is not provided.
#' @return If \code{outfile} is specified, the resulting \code{Raster*} object
#'   is saved as to that path. If \code{return_rast} is \code{TRUE} or
#'   \code{outfile} is not specified the resulting \code{RasterLayer} is
#'   returned, otherwise \code{NULL} is returned invisibly.
#' @importFrom raster stack writeRaster
#' @export

establishment_likelihood <- function(total_arrivals, suitability, outfile, 
  return_rast = FALSE) {

  out <- raster::stack(total_arrivals) * raster::stack(suitability)
  # note that suitability will only be one layer, but we use `stack` so that 
  # it correctly handles either a character file path and a raster object.

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
