#' Calculate establishment likelihood captured in top n cells
#'
#' Calculate the proportion of establishment likelihood captured in top n cells.
#'
#' @param x `Raster*` or `SpatRaster` object, or a character vector of file
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

  if(missing(layer_names) || length(layer_names != dim(x)[3])) {
    layer_names <- names(x)
  }
  if(any(duplicated(layer_names))) {
    stop('layer_names must be unique and must have length equal to the number of layers of x.',
         call.=FALSE)
  }

  n_notna <- terra::global(!is.na(x), sum)$sum
  n <- pmin(n_cells, n_notna)
  d <- terra::as.data.frame(x, cells=TRUE)
  q <- mapply(quantile, d[, -1], pmax(0, 1 - n_cells/n_notna))
  i <- mapply(function(test, v, q_) {
    if(test) d$cell[which(v >= q_)] else d$cell[which(v > q_)]
  }, n_cells >= n_notna, d[, -1], q, SIMPLIFY=FALSE)

  vals <- mapply(function(v, i_) v[i_], d[, -1], i, SIMPLIFY=FALSE)
  total <- terra::global(x, sum)$sum

  if (isTRUE(all)) {
    risk_captured <- lapply(vals, function(x) cumsum(sort(x, decreasing=TRUE)))
    proportion <- mapply(function(risk, tot) risk/tot, risk_captured, total)
    data.frame(map=rep(layer_names, each=nrow(proportion)),
               ncell=rep(seq_len(nrow(proportion)), ncol(proportion)),
               proportion = c(proportion))
  } else {
    proportion <- mapply(function(v, tot) sum(v)/tot, vals, total)
    data.frame(map = layer_names, ncell = n, proportion = proportion)
  }
}
