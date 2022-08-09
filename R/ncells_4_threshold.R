#' Extract number of cells required to meet threshold proportion of risk
#'
#' Extract number of cells required to meet threshold proportion of risk.
#'
#' @param risk_rasters `Raster*`, [`SpatRaster`], or character vector of one or
#'   more pathers to raster files.
#' @param names Character. Optional names corresponding to `risk_rasters`. If
#'   not provided, layer names are used.
#' @param proportion_captured Numeric vector. Proportion(s) of risk to be
#'   captured.
#' @return A `tibble` containing the number of cells requiring surveillance in
#'   order to capture the proportion(s) of total risk given by
#'   `proportion_captured`.
#' @importFrom terra rast values global
#' @importFrom tidyr spread
#' @export
ncells_4_threshold <- function(risk_rasters, layer_names,
  proportion_captured = c(0.6, 0.8, 0.9, 0.95)) {

  if(is.character(risk_rasters) || is(risk_rasters, 'Raster')) {
    risk_rasters <- terra::rast(risk_rasters)
  } else if(!is(risk_rasters, 'SpatRaster')) {
    stop('risk_rasters must be a Raster*, SpatRaster, or character vector ',
         'of raster file paths.', call.=FALSE)
  }

  if(missing(layer_names)) {
    layer_names <- names(rast)
  } else if(length(layer_names) != dim(rast)[3]) {
    stop('length of `names` not equal to the number of raster layers.',
         call.=FALSE)
  }
  if(any(duplicated(layer_names))) stop('names must be unique.', call.=FALSE)

  vals <- terra::values(rast, dataframe=TRUE)
  total <- terra::global(rast, sum)$sum

  propn <- mapply(function(v, tot) {
    cumsum(sort(v, decreasing=TRUE))/tot
  }, vals, total, SIMPLIFY=FALSE)

  cells <- lapply(propn, function(x) {
    sapply(proportion_captured, function(p) {
      if(p==0) 0 else which(x >= p)[1]
    })
  })

  dat <- data.frame(Map=rep(layer_names, each=length(proportion_captured)),
                    Proportion=rep(proportion_captured, length(layer_names)),
                    n_cells=unlist(cells))

  tidyr::spread(dat, Proportion, n_cells)
}
