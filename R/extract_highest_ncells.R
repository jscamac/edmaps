#' Returns a raster with values for the n cells with highest establishment
#' likelihood
#'
#' Returns a raster with values only for the n cells with highest establishment
#' likelihood.
#'
#' @param x `RasterLayer`, [`SpatRaster`], or file path to a raster file
#'   containing estimated likelihoods of establishment or arrival.
#' @param n_cells Integer. The number of cells to return.
#' @param outfile Character. Output raster file path. If not provided, the
#'   resulting [`SpatRaster`] will be returned to R.
#' @param return_rast Logical. Should the [`SpatRaster`] be returned to R?
#'   Ignored if `outfile` is not provided.
#' @return If `outfile` is specified, the resulting [`SpatRaster`] is saved as
#'   to that path. If `return_rast` is `TRUE` or `outfile` is not specified, the
#'   resulting [`SpatRaster`] is returned, otherwise `NULL` is returned
#'   invisibly.
#' @details This function returns a raster containing values for only the top
#'   _n_ cells.
#' @section Warning: This function uses the quantile approach for determining
#'   the top _n_ cells. As such, if risk is highly aggregated this function may
#'   run into issues when `n_cells` is high.
#' @importFrom terra rast global as.data.frame writeRaster
#' @importFrom methods is
#' @export
extract_highest_ncells <- function(x, n_cells, outfile, return_rast=FALSE) {

  if(is.character(x) || is(x, 'RasterLayer')) {
    x <- terra::rast(x)
  } else if(!is(x, 'SpatRaster') || dim(x)[3] > 1) {
    stop('x must be a RasterLayer, single-layered SpatRaster, ',
         'or vector of raster file paths.')
  }

  # Convert all cells not captureid by top n_cells to NA
  q <- quantile(terra::as.data.frame(r)[[1]],
           1 - n_cells/terra::global(r, function(x) sum(!is.na(x)))[[1]])
  r[r < q] <- NA

  if(!missing(outfile)) {
    # Create directory if it does not exist
    if(!dir.exists(dirname(outfile))) {
      dir.create(dirname(outfile), recursive = TRUE)
    }
    # write out raster
    terra::writeRaster(r, outfile, overwrite=TRUE)
  }
  if(isTRUE(return_rast) || missing(outfile)) {
    r
  } else {
    invisible(NULL)
  }
}
