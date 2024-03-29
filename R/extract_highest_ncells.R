#' Returns a raster with values for the n cells with highest establishment
#' likelihood
#'
#' Returns a raster with values only for the n cells with highest establishment
#' likelihood.
#'
#' @param infile File path to a raster containing estimated likelihoods of
#'   establishment or arrival.
#' @param n_cells Integer. The number of cells to return.
#' @param outfile Character. Output raster file path. If not provided, the
#'   `RasterLayer` will be returned to R.
#' @param return_rast Logical. Should the `RasterLayer` be returned to R?
#'   Ignored if `outfile` is not provided.
#' @return If `outfile` is specified, the resulting `RasterLayer` is
#'   saved as to that path. If `return_rast` is `TRUE` or
#'   `outfile` is not specified, the resulting `RasterLayer` is
#'   returned, otherwise `NULL` is returned invisibly.
#' @details This function returns a raster containing values for only the top
#'   _n_ cells.
#' @section Warning: This function uses the quantile approach for determining
#'   the top _n_ cells. As such, if risk is highly aggregated this function may
#'   run into issues when `n_cells` is high.
#' @importFrom raster raster quantile cellStats writeRaster
#' @export
extract_highest_ncells <- function(infile, n_cells, outfile,
  return_rast=FALSE) {
  r <- raster::raster(infile)
  # Convert all cells not captured by top n_cells to NA
  r[r < raster::quantile(
    r, 1 - n_cells/raster::cellStats(!is.na(r), sum)
  )] <- NA

  if(!missing(outfile)) {
    # Create directory if it does not exist
    if(!dir.exists(dirname(outfile))) {
      dir.create(dirname(outfile), recursive = TRUE)
    }
    # write out raster
    raster::writeRaster(r, outfile, overwrite=TRUE)
  }
  if(isTRUE(return_rast) || missing(outfile)) {
    r
  } else {
    invisible(NULL)
  }
}
