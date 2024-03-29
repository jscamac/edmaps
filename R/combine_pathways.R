#' Calculate the probability of arrival across pathways
#'
#' Calculate the probability of arrival across pathways.
#' @param x Either a `RasterStack` or a character vector giving file
#'   path(s) to rasters. Each raster should describe a pathway to be included in
#'   the summation, and cell values give the probability of arrival for the
#'   corresponding pathway. All rasters must have the same extent and
#'   resolution.
#' @param outfile Character. Output raster file path. If not provided, the
#'   `RasterLayer` will be returned to R.
#' @param return_rast Logical. Should the `RasterLayer` be returned to R?
#'   Ignored if `outfile` is not provided.
#' @return If `outfile` is specified, the resulting `RasterLayer` is
#'   saved to `outfile`. If `return_rast` is `TRUE` or
#'   `outfile` is not specified, the resulting `RasterLayer` is
#'   returned, otherwise `NULL` is returned invisibly.
#' @importFrom raster stack writeRaster
#' @export
combine_pathways <- function(x, outfile, return_rast = FALSE) {

  pathways <- raster::stack(x)

  out <- 1 - prod(1 - pathways)

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
