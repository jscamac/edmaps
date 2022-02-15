#' Calculate establishment likelihood captured in top n cells
#'
#' Calculate the proportion of establishment likelihood captured in top n cells.
#'
#' @param infiles Character vector. File path(s) to one or more raster files.
#' @param names Character vector. Names corresponding to `infiles`.
#' @param n_cells Integer. The number of cells to consider.
#' @param all Logical. If `TRUE`, return the proportion captured from 1 to _n_
#'   cells. If `FALSE`, return the cumulative proportion.
#' @return Proportion of establishment likelihood captured, or a vector of
#'   cumulative proportions.
#' @importFrom raster stack unstack Which quantile cellStats
#' @importFrom dplyr bind_rows
#' @export
captured_by_ncells <- function(infiles, names, n_cells, all=TRUE) {

  lst <- lapply(seq_along(infiles), function(x) {
    rast <- raster::stack(infiles[x])
    n_notna <- raster::cellStats(!is.na(rast), sum)
    n <- min(n_cells, n_notna)
    q <- mapply(raster::quantile, raster::unstack(rast),
                pmax(0, 1 - n_cells/n_notna))
    i <- mapply(function(test, r, q_) {
      if(test) {
        raster::Which(r >= q_, cells=TRUE)
      } else {
        raster::Which(r > q_, cells=TRUE)
      }
    }, n_cells >= n_notna, raster::unstack(rast), q, SIMPLIFY=FALSE)

    vals <- mapply(function(r, i_) r[i_], raster::unstack(rast), i,
                   SIMPLIFY=FALSE)
    total <- raster::cellStats(rast, sum)

    if (isTRUE(all)) {
      risk_captured <- lapply(vals, function(x) cumsum(sort(x, decreasing=TRUE)))
      proportion <- mapply(function(risk, tot) risk/tot, risk_captured, total)
      data.frame(map = rep(names[x], n), ncell = seq_len(n),
                 proportion = proportion)
    } else {
      proportion <- mapply(function(v, tot) sum(v)/tot, vals, total,
                           SIMPLIFY=FALSE)
      data.frame(map = names[x], ncell = n,
                 proportion = do.call(cbind, proportion))
    }
  })
  as.data.frame(dplyr::bind_rows(lst))
}
