#' Downscale raster resolution
#'
#' Aggregate raster cells (and optionally layers) to coarser resolution.
#'
#' @param rast Raster* object or file path to a raster file.
#' @param outfile Character. Output raster file path. If not provided, object 
#'   will be returned to R. Directory will be created  recursively if it does 
#'   not exist.
#' @param aggregate_factor Integer. Aggregation factor expressed as number of
#'   cells in each direction (horizontally and vertically). Or a vector of two
#'   integers (horizontal and vertical aggregation factors) or three integers
#'   (when also aggregating over layers).
#' @param fun Function. Function used to aggregate values. Default is
#'   \code{sum}. 
#' @param return_rast Logical. Should the resulting raster be returned to R?
#'   Ignored if \code{outfile} is not provided.
#' @return If \code{outfile} is specified, the resulting raster is saved as a
#'   geotiff to that path. If \code{return_rast} is \code{TRUE} or
#'   \code{outfile} is not specified the resulting raster is returned,
#'   otherwise \code{NULL} is returned invisibly.
#' @importFrom raster raster aggregate writeRaster
#' @export
aggregate_raster <- function(rast, outfile, aggregate_factor, fun = sum, 
  return_rast = FALSE) {
  if(missing(aggregate_factor)) {
    stop('aggregate_factor must be provided')
  }

  if(is.character(rast)) rast <- raster::raster(rast)
  out <- raster::aggregate(rast, fact = aggregate_factor, fun = fun)

  # Create directory if it does not exist
  if(!missing(outfile)) {
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
