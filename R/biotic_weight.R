#' Combine abiotic and biotic layers to create suitability raster
#'
#' Combine biotic (and abiotic) layers into a single suitability raster to be
#' used to scale arrival rates.
#'
#' @param x One of: a [`SpatRaster`] object; a `RasterLayer`, a list of
#'   [`SpatRaster`] objects, or a vector of file paths to raster files.
#' @param outfile Character. Filename where output will be saved. If not
#'   provided, [`SpatRaster`] object will be returned to R.
#' @param return_rast Logical. Should the [`SpatRaster`] object be returned to
#'   R? Ignored if `outfile` is not provided.
#' @return If `outfile` is specified, the resulting [`SpatRaster`] object is
#'   saved as to that path. If `return_rast` is `TRUE` or `outfile` is not
#'   specified the resulting [`SpatRaster`] is returned, otherwise `NULL` is
#'   returned invisibly.
#' @importFrom terra rast setMinMax minmax writeRaster
#' @importFrom methods is
#' @export
suitability <- function(x, outfile, return_rast = FALSE) {

  out <- if(is.character(x) ||
            is(x, 'RasterLayer') ||
            is.list(x) && all(sapply(x, function(r) is(r, 'SpatRaster')))) {
    terra::rast(x)
  } else {
    x
  }
  terra::setMinMax(out)

  # Check layers are all between zero and 1
  if(min(terra::minmax(out)[1, ]) < 0 || max(terra::minmax(out)[2, ]) > 1) {
    stop("Not all layers are normalised to be within 0 and 1")
  }

  if(dim(out)[3] > 1) out <- prod(out)

  if(!missing(outfile)) {
    # Create directory if it does not exist
    if(!dir.exists(dirname(outfile))) {
      dir.create(dirname(outfile), recursive = TRUE)
    }
    # write out raster
    terra::writeRaster(out, outfile, overwrite = TRUE)
  }
  if(isTRUE(return_rast) || missing(outfile)) {
    out
  } else {
    invisible(NULL)
  }
}
