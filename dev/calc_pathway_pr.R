#' Calculate the pathway-specific probability of pest arrival
#'
#' Calculate the pathway-specific probability of pest arrival for each
#' raster cell.
#' @param EE data.frame object obtained from \code{\link{calc_EE}}
#' @param rast Raster object or path to raster file containing dispersal weights.
#' @param outfile Character. Output raster file path. If not provided, the 
#' \code{RasterLayer} will be returned to R.
#' @param return_rast Logical. Should the \code{RasterLayer} be returned to R?
#'   Ignored if \code{outfile} is not provided.
#' @return If \code{outfile} is specified, the resulting \code{RasterLayer} is 
#'   saved to \code{outfile}. If \code{return_rast} is \code{TRUE} or 
#'   \code{outfile} is not specified, the resulting \code{RasterLayer} is 
#'   returned, otherwise \code{NULL} is returned invisibly.
#' @importFrom raster raster stack writeRaster
#' @export
calc_pathway_pr <- function(EE, rast, outfile, return_rast) {
  
  if(is.character(rast)) {
    rast <- raster::raster(rast)
  }
  
  out <- 1-sum(raster::stack(lapply(1:nrow(EE), function(x) {
    # Calculate the probability the cell is unoccupied
    EE$probability[x] * (1-rast)^EE$N_incursions[x]
  })
  ))
  
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