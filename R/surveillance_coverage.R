#' Calculate the proportion of total establishment likelihood captured by
#' current surveillance
#'
#' Calculate the proportion of total establishment likelihood captured by 
#' current surveillance.
#'
#' @param establishment_rasters Character vector. A vector of one or more file 
#'   path to rasters describing establishment likelihood.
#' @param layer_names Character. Name to be assigned to each establishment
#'   raster. If not specified, names of the raster layers will be used.
#' @param surveillance_locations Character. Path to a csv file containing 
#'   surveillance locations, including columns titled "Longitude" and 
#'   "Latitude" with coordinates given in decimal degrees (GDA94).
#' @return The proportion of establishment likelihood captured by current
#'   surveillance.
#' @importFrom raster raster extract cellStats
#' @importFrom dplyr filter bind_rows
#' @importFrom sp coordinates proj4string CRS spTransform
#' @export
surveillance_coverage <- function(establishment_rasters, layer_names,
  surveillance_locations) {
  dat <- lapply(seq_along(establishment_rasters), function(i) {
    ras <- raster::raster(establishment_rasters[i])
    
    locs <- read.csv(surveillance_locations) %>%
      dplyr::filter(!is.na(Latitude) & !is.na(Longitude)) %>%
      {sp::coordinates(.) <- c("Longitude", "Latitude"); .} %>%
      {sp::proj4string(.) <- sp::CRS("+init=epsg:4283"); .} %>%
      # ^ provide lat/long proj string
      sp::spTransform(., sp::CRS(sp::proj4string(ras)))
    
    loc_cells <- unique(raster::extract(ras, locs, cellnumbers=TRUE))
    total <- raster::cellStats(ras, sum)
    data.frame(Map = ifelse(is.null(layer_names), names(ras),layer_names[i]), 
               Proportion = round(sum(loc_cells[, 2], na.rm=TRUE)/total, 2), 
               stringsAsFactors = FALSE)
  })
  dplyr::bind_rows(dat)
}
