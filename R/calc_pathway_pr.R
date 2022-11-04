#' Calculate the pathway-specific probability of pest arrival
#'
#' Calculate the pathway-specific probability of pest arrival for each raster
#' cell.
#' @param EE `data.frame` object obtained from [calc_establishment()].
#' @param rast RasterLayer object, single-layer SpatRaster object, or character
#'   path to a single raster file containing dispersal weights.
#' @param outfile Character. Output raster file path. If not provided, the
#'   [`SpatRaster`] object will be returned to R.
#' @param return_rast Logical. Should the [`SpatRaster`] object be returned to
#'   R? Ignored if `outfile` is not provided.
#' @return If `outfile` is specified, the resulting [`SpatRaster`] object is
#'   saved to `outfile`. If `return_rast` is `TRUE` or `outfile` is not
#'   specified, the resulting [`SpatRaster`] object is returned, otherwise
#'   `NULL` is returned invisibly.
#' @importFrom dplyr mutate
#' @importFrom terra as.data.frame rast writeRaster init
#' @importFrom methods is
#' @importFrom stats setNames
#' @export
calc_pathway_pr <- function(EE, rast, outfile, return_rast=TRUE) {

  if(any(EE$probability < 0 | EE$probability > 1)) {
    stop('Probabilities passed to `EE` must be in the range 0-1.')
  }

  if(is(rast, 'RasterLayer') || is.character(rast) && length(rast) == 1) {
    rast <- terra::rast(rast)
  } else if(!is(rast, 'SpatRaster') || dim(rast)[3] > 1) {
    stop('rast must be a  must be a single-layer SpatRaster object, ',
         'a RasterLayer object, or a single file path to a raster file.')
  }

  d <- terra::as.data.frame(rast, cells=TRUE) %>%
    stats::setNames(c('cell', 'value')) %>%
    dplyr::mutate(value = 1 - value, prob_absent=0)
  # ^ Transform to probability (given incursion) that pest does *not* arrive at
  # cell (i.e. arrives at some other cell)

  if(any(d$value > 1)) stop('dispersal weights in `rast` must not be negative.')

  # Loop over rows of EE, calculating the probability the cell does *not*
  # receive the pest, given the number of incursions and corresponding
  # probability of that number of incursions.
  for(i in seq_len(nrow(EE))) {
    d$prob_absent <- d$prob_absent + EE$probability[i] *
      d$value^EE$N_incursions[i]
  }
  d$prob_absent <- pmin(pmax(d$prob_absent, 0), 1)

  # Initialise raster and populate with probability of presence
  out <- terra::init(rast, fun=0)
  out[d$cell] <- ifelse(is.na(d$prob_absent), 0, 1 - d$prob_absent)


  if(!missing(outfile)) {
    if(!dir.exists(dirname(outfile))) {
      dir.create(dirname(outfile), recursive = TRUE)
    }
    terra::writeRaster(out, outfile, overwrite = TRUE)
  }
  if(isTRUE(return_rast) || missing(outfile)) {
    out
  } else {
    invisible(outfile)
  }
}
