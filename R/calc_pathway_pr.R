#' Calculate the pathway-specific probability of pest arrival
#'
#' Calculate the pathway-specific probability of pest arrival for each raster
#' cell.
#' @param EE \code{data.frame} object obtained from \code{\link{calc_EE}}.
#' @param rast Raster object or character path to raster file containing
#'   dispersal weights.
#' @param outfile Character. Output raster file path. If not provided, the
#'   \code{RasterLayer} will be returned to R.
#' @param return_rast Logical. Should the \code{RasterLayer} be returned to R?
#'   Ignored if \code{outfile} is not provided.
#' @return If \code{outfile} is specified, the resulting \code{RasterLayer} is
#'   saved to \code{outfile}. If \code{return_rast} is \code{TRUE} or
#'   \code{outfile} is not specified, the resulting \code{RasterLayer} is
#'   returned, otherwise \code{NULL} is returned invisibly.
#' @importFrom dplyr mutate row_number
#' @importFrom raster as.data.frame mask raster writeRaster
#' @importFrom stats na.omit
#' @export
calc_pathway_pr <- function(EE, rast, outfile, return_rast=TRUE) {

  if(is.character(rast)) {
    rast <- raster::raster(rast)
  }

  d <- dplyr::mutate(setNames(raster::as.data.frame(rast), 'value'),
                     cell=dplyr::row_number()) %>%
    stats::na.omit() %>%
    dplyr::mutate(value = 1 - value, prob_absent=0)
  # ^ Transform to probability (given incursion) that pest does *not* arrive at
  # cell (i.e. arrives at some other cell)

  # Loop over rows of EE, calculating the probability the cell does *not*
  # receive the pest, given the number of incursions and corresponding
  # probability of that number of incursions.
  for(i in seq_len(nrow(EE))) {
    d$prob_absent <- d$prob_absent + EE$probability[i] *
      d$value^EE$N_incursions[i]
  }

  # Initialise raster and populate with probability of presence
  out <- raster::mask(raster::raster(rast), rast)
  out[d$cell] <- 1 - d$prob_absent


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
