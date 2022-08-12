#' Calculate the proportion of total establishment likelihood captured by
#' current surveillance
#'
#' Calculate the proportion of total establishment likelihood captured by
#' current surveillance.
#'
#' @param ras `Raster*`, [`SpatRaster`], or a character vector of one or more
#'   paths to raster files describing establishment likelihood.
#' @param layer_names Character. Name to be assigned to each establishment
#'   raster. If not specified, names of the raster layers will be used.
#' @param surveillance_locations Character. Path to a csv file containing
#'   surveillance locations, including columns titled "Longitude" and
#'   "Latitude" with coordinates given in decimal degrees (GDA94).
#' @return A `data.frame` with the proportion of establishment likelihood
#'   captured by current surveillance.
#' @importFrom terra rast extract global project vect crs
#' @importFrom dplyr filter bind_rows
#' @export
surveillance_coverage <- function(ras, layer_names, surveillance_locations) {

  if(is.character(ras) || is(ras, 'Raster')) {
    ras <- terra::rast(ras)
  } else if(!is(ras, 'SpatRaster')) {
    stop('ras must be a Raster* or SpatRaster object, or a character vector ',
         'of one or more paths to raster files.')
  }

  locs <- read.csv(surveillance_locations) %>%
    dplyr::filter(!is.na(Latitude) & !is.na(Longitude)) %>%
    terra::vect(geom=c('Longiture', 'Latitude'), crs='epsg:4283') %>%
    terra::project(terra::crs(ras))

  loc_cells <- unique(terra::extract(ras, locs))
  total <- terra::global(ras, sum)$sum

  data.frame(Map = if (missing(layer_names)) names(ras) else layer_names,
             Proportion = round(colSums(loc_cells)/total, 2),
             stringsAsFactors = FALSE)
}
