#' Combine abiotic and biotic layers to create suitability raster
#'
#' Combine biotic (and abiotic) layers into a single suitability raster to be
#' used to scale arrival rates.
#'
#' @param x One of: a \code{RasterStack}; a \code{RasterBrick}; a list of 
#'   \code{RasterLayer} objects, or a vector of file paths to raster files.
#' @param outfile Character. Name of geotiff where output will be saved. If not
#'   provided, raster object will be returned to R.
#' @param return_rast Logical. Should the raster object be returned to R?
#'   Ignored if \code{outfile} is not provided.
#' @return If \code{outfile} is specified, the resulting raster is saved as a
#'   geotiff to that path. If \code{return_rast} is \code{TRUE} or
#'   \code{outfile} is not specified the resulting raster is returned, otherwise
#'   \code{NULL} is returned invisibly.
#' @importFrom raster stack setMinMax minValue maxValue writeRaster
#' @export
suitability <- function(x, outfile, return_rast = FALSE) {

  if(is.list(x)) {
    out <- raster::stack(x)
  } else if(is.character(x)) {
    out <- raster::stack(x)
  } else {
    out <- x
  }
  out <- raster::setMinMax(out)

  # Check layers are all between zero and 1
  if(min(raster::minValue(out)) < 0 || max(raster::maxValue(out)) > 1) {
    stop("Not all layers are normalised to be within 0 and 1")
  }

  if(class(out) %in% c("RasterStack", "RasterBrick") && 
     raster::nlayers(out) > 1) {
    out <- prod(out)
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
