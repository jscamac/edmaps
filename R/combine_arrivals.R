#' Sum arrivals across entry pathways
#'
#' Sums estimated arrivals rates across all entry pathways.
#' 
#' @param x Character vector giving file path(s) to rasters to be included in 
#'   the summation, or a \code{RasterStack} containing those rasters. If a 
#'   character vector of length > 1 is provided, all rasters must have the 
#'   same extent and resolution.
#' @param outfile Character. Output raster file path. If not provided, the 
#'   \code{RasterLayer} will be returned to R.
#' @param return_rast Logical. Should the \code{RasterLayer} be returned to R?
#'   Ignored if \code{outfile} is not provided.
#' @return If \code{outfile} is specified, the resulting \code{RasterLayer} is 
#'   saved to \code{outfile}. If \code{return_rast} is \code{TRUE} or 
#'   \code{outfile} is not specified, the resulting \code{RasterLayer} is 
#'   returned, otherwise \code{NULL} is returned invisibly.
#' @importFrom raster stack writeRaster
#' @importFrom methods is
#' @export
combine_arrivals <- function(x, outfile, return_rast = FALSE) {
  if(is.character(x)) {
    x <- raster::stack(x)
  } else if(!is(x, 'RasterStack')) {
    stop('x must be either a character vector of file paths, or a RasterStack.')
  }
  out <- sum(x, na.rm = TRUE)

  if(!missing(outfile)) {
    # Create directory if it does not exist
    if(!dir.exists(dirname(outfile))) {
      dir.create(dirname(outfile), recursive = TRUE)
    }
    # write out raster
    raster::writeRaster(out, outfile, overwrite = TRUE)
  }
  if(isTRUE(return_rast) || missing(outfile)) {
    names(out) <- "total_arrivals"
    out
  } else {
    invisible(NULL)
  }
}
