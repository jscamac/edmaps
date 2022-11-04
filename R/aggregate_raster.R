#' Downscale raster resolution
#'
#' Aggregate raster cells (and optionally layers) to coarser resolution.
#'
#' @param rast [`SpatRaster`] or `Raster*` object, or file path to a raster file.
#' @param outfile Character. Output raster file path. If not provided, object
#'   will be returned to R. Directory will be created  recursively if it does
#'   not exist.
#' @param aggregate_factor Integer. Aggregation factor expressed as number of
#'   cells in each direction (horizontally and vertically). Or a vector of two
#'   integers (horizontal and vertical aggregation factors) or three integers
#'   (when also aggregating over layers).
#' @param fun Function. Function used to aggregate values. Default is
#'   `sum`. The function should accept a `na.rm` argument, or ignore it as a
#'   `...` argument.
#' @param return_rast Logical. Should the resulting [`SpatRaster`] object be
#'   returned to R? Ignored if `outfile` is not provided.
#' @param overwrite Should `outfile` be overwritten if it exists? Default is
#'   `TRUE`.
#' @return If `outfile` is specified, the resulting [`SpatRaster`] object is
#'   saved to that path. If `return_rast` is `TRUE` or `outfile` is not
#'   specified the resulting [`SpatRaster`] object is returned, otherwise
#'   `outfile` is returned invisibly.
#' @importFrom terra rast aggregate writeRaster
#' @importFrom methods is
#' @export
aggregate_raster <- function(rast, outfile, aggregate_factor, fun = sum,
  return_rast = FALSE, overwrite = TRUE) {

  if(missing(aggregate_factor)) {
    stop('aggregate_factor must be provided')
  }

  if(is.character(rast) || is(rast, 'Raster')) rast <- terra::rast(rast)
  out <- terra::aggregate(rast, fact = aggregate_factor, fun = fun)

  # Create directory if it does not exist
  if(!missing(outfile)) {
    if(!dir.exists(dirname(outfile))) {
      dir.create(dirname(outfile), recursive = TRUE)
    }
    # write out raster
    terra::writeRaster(out, filename = outfile, overwrite = overwrite)
  }
  if(isTRUE(return_rast) || missing(outfile)) {
    out
  } else {
    invisible(outfile)
  }
}
