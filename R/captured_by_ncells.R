#' Calculate establishment likelihood captured in top n cells
#'
#' Calculate the proportion of establishment likelihood captured in top n cells.
#'
#' @param x `Raster*` or [`SpatRaster`] object, or a character vector of file
#'   path(s) to one or more raster files.
#' @param layer_names Character vector. Optional names corresponding to `x`.
#' @param n_cells Integer. The number of cells to consider.
#' @param all Logical. If `TRUE`, return the proportion captured from 1 to _n_
#'   cells. If `FALSE`, return the cumulative proportion.
#' @return Proportion of establishment likelihood captured, or a vector of
#'   cumulative proportions.
#' @importFrom terra rast as.data.frame global
#' @export
captured_by_ncells <- function(x, layer_names, n_cells, all=TRUE) {
  if(is.character(x) || is(x, 'Raster')) {
    x <- terra::rast(x)
  } else if(!is(x, 'SpatRaster')) {
    stop('x must be a Raster* or SpatRaster object, ',
         'or a vector of raster file paths.', call.=FALSE)
  }
  if (missing(layer_names) || length(layer_names) != dim(x)[3]) {
    layer_names <- names(x)
  }
  if(any(duplicated(layer_names))) {
    stop('layer_names must be unique and must have length equal to the number of layers of x.',
         call.=FALSE)
  }

  n_notna <- terra::global(!is.na(x), sum)$sum
  n_cells <- pmin(n_cells, n_notna)
  total <- terra::global(x, sum)$sum
  d <- terra::as.data.frame(x)
  cap <- mapply(function(p, n, tot) {
    # na.last=NA removes NAs
    sort(p, na.last=NA, decreasing = TRUE)[seq_len(n)]/tot
  }, d, n_cells, total, SIMPLIFY = FALSE)

  if(all) {
    data.frame(map = rep(layer_names, lengths(cap)),
               ncell = unlist(lapply(n_cells, seq_len)),
               proportion = unlist(lapply(cap, cumsum), use.names = FALSE))
  } else {
    data.frame(map = layer_names, ncell = n, proportion = sapply(cap, sum))
  }
}
