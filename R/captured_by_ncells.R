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
#' @importFrom raster raster Which quantile cellStats
#' @importFrom dplyr bind_rows
#' @export
captured_by_ncells <- function(infiles, names, n_cells, all=TRUE) {
  
  lst <- lapply(seq_along(infiles), function(x) {
    raster <- raster::raster(infiles[x])
    i <- raster::Which(raster > raster::quantile(
      raster, 1 - n_cells/raster::cellStats(!is.na(raster), sum)), cells=TRUE)
    vals <- raster[i]
    total <- raster::cellStats(raster, sum)
    
    if (isTRUE(all)) {
      risk_captured <- cumsum(sort(vals, decreasing=TRUE))
      proportion <- risk_captured/total
      data.frame(map = rep(names[x], n_cells), ncell = seq_len(n_cells), 
                 proportion = proportion)
    } else {
      proportion <- sum(vals)/total
      data.frame(map = names[x], ncell = n_cells, proportion = proportion)
    }
  })
  as.data.frame(dplyr::bind_rows(lst))
}
