#' Sum arrivals across entry pathways
#'
#' Sums estimated arrivals rates across all entry pathways.
#'
#' @param x Character vector giving file path(s) to rasters to be included in
#'   the summation. All rasters must have the same extent and resolution.
#' @param outfile Character. Output raster file path. If not provided, the
#'   `RasterLayer` will be returned to R.
#' @param summarise_uncertainty Logical. If `TRUE`, and if
#'   `probability` has length > 1, the arrival frequency surfaces for the
#'   values of `probability` will be summarised to their median, min, and
#'   max cell-wise values (in that order).
#' @param return_rast Logical. Should the `RasterLayer` be returned to R?
#'   Ignored if `outfile` is not provided.
#' @return If `outfile` is specified, the resulting `RasterLayer` is
#'   saved to `outfile`. If `return_rast` is `TRUE` or
#'   `outfile` is not specified, the resulting `RasterLayer` is
#'   returned, otherwise `NULL` is returned invisibly. If
#'   `summarise_uncertainty` is `TRUE` and rasters passed to `x` are
#'   multiband, then the resulting stack will have three layers equal to the
#'   cell-wise median, minimum, and maximum of the layers, respectively.
#' @importFrom raster stack writeRaster calc
#' @importFrom methods is
#' @importFrom magrittr "%>%"
#' @export
combine_arrivals <- function(x, outfile, summarise_uncertainty=FALSE,
                             return_rast = FALSE) {

  nbands <- unique(sapply(x, function(f) dim(raster::stack(f))[3]))
  if(length(nbands) != 1) {
    stop('When multiple values are passed for prob_*, the same number of values ',
         'must be passed for each prob_* argument that is used.')
  }

  out <- lapply(seq_len(nbands), function(i) {
    message('Summing establishment likelihood across pathways for ',
            'leakage rate instance ', i, ' of ', nbands, '.')
    lapply(x, function(f) {
      raster::raster(f, band=i)
    }) %>% raster::stack() %>%
      sum(na.rm=TRUE)
  }) %>% raster::stack()

  if(isTRUE(summarise_uncertainty)) {
    out <- raster::stack(
      list(median=raster::calc(out, median),
           min=raster::calc(out, min),
           max=raster::calc(out, max))
    )
  }

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
