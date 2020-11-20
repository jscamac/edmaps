#' Calculate establishment likelihood captured in top n cells
#'
#' Calculate the proportion of establishment likelihood captured in top n cells.
#' 
#' @param infiles Character vector. File path(s) to one or more raster files.
#' @param names Character vector. Names corresponding to \code{infiles}.
#' @param n_cells Integer. The number of cells to consider.
#' @param all Logical. If \code{TRUE}, return the proportion captured from 1 to 
#'   \emph{n} cells. If \code{FALSE}, return the cumulative proportion.
#' @return Proportion of establishment likelihood captured, or a vector of
#'   cumulative proportions.
#' @importFrom raster stack unstack Which quantile cellStats
#' @importFrom dplyr bind_rows
#' @export
captured_by_ncells <- function(infiles, names, n_cells, all=TRUE) {
  
  lst <- lapply(seq_along(infiles), function(x) {
    rast <- raster::stack(infiles[x])
    n_notna <- raster::cellStats(!is.na(rast), sum)
    q <- mapply(raster::quantile, raster::unstack(rast), 
                pmax(0, 1 - n_cells/n_notna))
    i <- lapply(raster::unstack(rast > q), raster::Which, cells=TRUE)
    vals <- mapply(function(r, i_) r[i_], raster::unstack(rast), i, 
                   SIMPLIFY=FALSE)
    total <- raster::cellStats(rast, sum)
    
    if (isTRUE(all)) {
      risk_captured <- lapply(vals, function(x) cumsum(sort(x, decreasing=TRUE)))
      proportion <- mapply(function(risk, tot) risk/tot, risk_captured, total)
      data.frame(map = rep(names[x], n_cells), ncell = seq_len(n_cells), 
                 proportion = proportion)
    } else {
      proportion <- mapply(function(v, tot) sum(v)/tot, vals, total, 
                           SIMPLIFY=FALSE)
      data.frame(map = names[x], ncell = n_cells, 
                 proportion = do.call(cbind, proportion))
    }
  })
  as.data.frame(dplyr::bind_rows(lst))
}
