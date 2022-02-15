#' Extract number of cells required to meet threshold proportion of risk
#'
#' Extract number of cells required to meet threshold proportion of risk.
#'
#' @param risk_rasters Character. File path(s) to rasters to be loaded.
#' @param names Character. Names corresponding to `infiles`.
#' @param proportion_captured Numeric vector. Proportion(s) of risk to be
#'   captured.
#' @return A `data.frame` containing the number of cells to be trapped to
#'   capture given proportion(s) of total risk across multiple input files.
#' @importFrom raster raster getValues cellStats
#' @importFrom dplyr bind_cols bind_rows
#' @importFrom tidyr spread
#' @export
ncells_4_threshold <- function(risk_rasters, names,
  proportion_captured = c(0.6, 0.8, 0.9, 0.95)) {

  extract_ncells <- function(infile, proportion_captured) {

    raster <- raster::raster(infile)
    vals <- raster::getValues(raster)
    total <- raster::cellStats(raster, sum)

    risk_captured <- cumsum(sort(vals, decreasing=TRUE))
    cells <- sapply(proportion_captured, function(x) {
      min(which(round(risk_captured/total, 2) == x))
    })
    data.frame(Proportion = proportion_captured, n_cells = cells)
  }

  dat <- lapply(seq_along(risk_rasters), function(x) {
    dplyr::bind_cols(data.frame(
      Map = rep(names[x], length(proportion_captured)), stringsAsFactors=FALSE),
      extract_ncells(risk_rasters[x], proportion_captured))
  })
  tidyr::spread(dplyr::bind_rows(dat), Proportion, n_cells)
}
