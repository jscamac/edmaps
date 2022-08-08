#' Calculate the probability of arrival across pathways
#'
#' Calculate the probability of arrival across pathways.
#' @param x A `RasterStack`, [`SpatRaster`], or a character vector giving file
#'   path(s) to raster files. Each raster should describe a pathway to be
#'   included in the summation, and cell values give the probability of arrival
#'   for the corresponding pathway. All rasters must have the same extent and
#'   resolution.
#' @param outfile Character. Output raster file path. If not provided, the
#'   [`SpatRaster`] will be returned to R.
#' @param return_rast Logical. Should the [`SpatRaster`] be returned to R?
#'   Ignored if `outfile` is not provided.
#' @return If `outfile` is specified, the resulting [`SpatRaster`] is
#'   saved to `outfile`. If `return_rast` is `TRUE` or
#'   `outfile` is not specified, the resulting [`SpatRaster`] is
#'   returned, otherwise `NULL` is returned invisibly.
#' @importFrom terra rast writeRaster
#' @importFrom methods is
#' @export
combine_pathways <- function(x, outfile, return_rast = FALSE) {

  if(is.character(x) || is(x, 'Raster')) {
    x <- terra::rast(x)
  } else if(!is(x, 'SpatRaster')) {
    stop('x must be a Raster* or SpatRaster object, ',
         'or a vector of raster file paths.', call.=FALSE)
  }

  out <- 1 - prod(1 - x)

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
