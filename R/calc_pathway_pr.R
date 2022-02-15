#' Calculate the pathway-specific probability of pest arrival
#'
#' Calculate the pathway-specific probability of pest arrival for each raster
#' cell.
#' @param EE `data.frame` object obtained from [calc_EE()].
#' @param rast Raster object or character path to raster file containing
#'   dispersal weights.
#' @param outfile Character. Output raster file path. If not provided, the
#'   `RasterLayer` will be returned to R.
#' @param return_rast Logical. Should the `RasterLayer` be returned to R?
#'   Ignored if `outfile` is not provided.
#' @return If `outfile` is specified, the resulting `RasterLayer` is
#'   saved to `outfile`. If `return_rast` is `TRUE` or
#'   `outfile` is not specified, the resulting `RasterLayer` is
#'   returned, otherwise `NULL` is returned invisibly.
#' @importFrom dplyr mutate row_number
#' @importFrom raster as.data.frame raster writeRaster init
#' @importFrom stats na.omit setNames
#' @export
calc_pathway_pr <- function(EE, rast, outfile, return_rast=TRUE) {

  if(is.character(rast)) {
    rast <- raster::raster(rast)
  }

  d <- dplyr::mutate(stats::setNames(raster::as.data.frame(rast), 'value'),
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
  out <- raster::init(rast, function(x) 0)
  out[d$cell] <- ifelse(is.na(d$prob_absent), 0, 1 - d$prob_absent)


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
