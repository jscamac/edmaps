#' Estimates pest arrivals through mail
#'
#' Estimates arrival rate attributable to mail as a function of population
#' density.
#'
#' @param pop_density A \code{RasterLayer} or file path to a raster file 
#'   containing population density.
#' @param total_mail Integer. Amount of mail.
#' @param probability Numeric. The probability a mail item contains pest.
#' @param outfile Character. Output raster file path. If not provided, raster 
#'   object will be returned to R.
#' @param return_rast Logical. Should the raster object be returned to R?
#'   Ignored if \code{outfile} is not provided.
#' @return If \code{outfile} is specified, the resulting raster is saved as a
#'   to that path. If \code{return_rast} is \code{TRUE} or \code{outfile} is 
#'   not specified the resulting raster is returned, otherwise \code{NULL} is 
#'   returned invisibly.
#' @family functions estimating arrivals
#' @importFrom raster raster writeRaster
#' @export
arrivals_by_mail <- function(pop_density, total_mail, probability, outfile, 
  return_rast=FALSE) {
  #Load raster
  if(is.character(pop_density)) {
    pop <- raster::raster(pop_density)
  } else if(is(pop_density, 'RasterLayer')) {
    pop <- pop_density
  } else {
    stop('pop_density must be a RasterLayer or a file path to a raster file.')
  }

  # Calculate proportion of population density
  prop_pop <- calc_proportion(pop)

  # Disperse mail and calculate arrival rate
  out <- disperse_arrivals(prop_pop, total_mail, probability)
  names(out) <- c("arrivals_by_mail")

  if(!missing(outfile)) {
    # Create directory if it does not exist
    if(!dir.exists(dirname(outfile))) {
      dir.create(dirname(outfile), recursive = TRUE)
    }
    # write out raster
    raster::writeRaster(out, outfile, overwrite=TRUE)
  }
  if(isTRUE(return_rast) || missing(outfile)) {
    out
  } else {
    invisible(NULL)
  }
}
