#' Calculate the probability of arrival across pathways
#' 
#' Calculate the probability of arrival across pathways
#' @param x Character vector giving file path(s) to rasters to be included in 
#'   the summation. All rasters must have the same extent and resolution.
#' @param outfile Character. Output raster file path. If not provided, the 
#'   \code{RasterLayer} will be returned to R.
#' @param return_rast Logical. Should the \code{RasterLayer} be returned to R?
#'   Ignored if \code{outfile} is not provided.
#' @return If \code{outfile} is specified, the resulting \code{RasterLayer} is 
#'   saved to \code{outfile}. If \code{return_rast} is \code{TRUE} or 
#'   \code{outfile} is not specified, the resulting \code{RasterLayer} is 
#'   returned, otherwise \code{NULL} is returned invisibly.
#' @importFrom raster stack writeRaster
#' @export


combine_pathways <- function(x, outfile, return_rast = FALSE) {
  
  pathways <- raster::stack(x)
  
  out <- 1 - prod(1-pathways)
  
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