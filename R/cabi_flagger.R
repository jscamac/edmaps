#' Flags records that occur in countries that are not reported in CABI
#'
#' Flags records that occur in countries that are not reported in CABI.
#' 
#' @param occurrence_records A \code{data.frame}, \code{sf} object or
#'   \code{SpatialPoints*} object, or a path to a .csv file containing the 
#'   locations of species of interest. (If a \code{data.frame} is provided,
#'   it must contain the named columns "Latitude" and "Longitude").
#' @param cabi_ref Character. The path to a .csv file downloaded from a CABI
#'   datasheet containing the country-scale distributional data for the pest.
#' @param manual_check Logical. Allows interactive selection of which 
#'   unverified points to retain. Interactive map will only be produced if 
#'   unverified records are present and if this argument is set to \code{TRUE}.
#' @param return_df Logical. Return the object as a \code{data.frame} or as an 
#'   \code{sf} object (i.e. \code{TRUE}, the default). 
#' @return A \code{data.frame} or an \code{sf} points object flagging records 
#'   found in countries in which CABI does not report the species as being 
#'   established.
#' @importFrom magrittr "%>%"
#' @importFrom rnaturalearth ne_countries
#' @importFrom utils read.csv
#' @importFrom sf st_as_sf st_crs st_intersection st_contains as_Spatial
#' @importFrom dplyr filter mutate select
#' @importFrom mapedit selectFeatures
#' @export

cabi_flagger <- function(occurrence_records, cabi_ref, manual_check = FALSE,
  return_df =FALSE) {
  
  world <- rnaturalearth::ne_countries(returnclass='sf',scale = 50)
  
  if(is.character(occurrence_records)) {
    occurrence_records <- utils::read.csv(occurrence_records)
  }
  
  occ <- suppressMessages(sf::st_as_sf(occurrence_records,
                      coords = c("Longitude","Latitude"),
                      crs = sf::st_crs(world)))
  
  cabi <- suppressMessages(sf::st_as_sf(read.csv(file = cabi_ref,skip = 2),
                       coords = c("Longitude","Latitude"),
                       crs = sf::st_crs(world)))
  
  # Get CABI country names
  cabi_countries <- suppressMessages(
    suppressWarnings(sf::st_intersection(cabi, world)$name)
  )
  
  # Get CABI country polygons
  country_polygons <- world %>%
    dplyr::filter(name %in% unique(cabi_countries))
  
  # Identify points that are present within CABI identified countries
  out <- suppressMessages(suppressWarnings(
    sf::st_join(occ, country_polygons, join = sf::st_intersects)
  )) %>%
    dplyr::mutate(cabi_verified = as.numeric(!is.na(name))) %>%
    dplyr::select(cabi_verified)
  
  
  # If unverified CABI records... check
  if(manual_check == TRUE) {
    
    if(sum(out$cabi_verified ==0) > 0) {
      
      message("Please select points to retain")
      unflagged <- suppressMessages(out %>%
        dplyr::filter(cabi_verified == 0) %>%
        mapedit::selectFeatures(
          ., title = "Select unverified records to retain"))
      
      out <- suppressMessages(out %>%
        dplyr::filter(cabi_verified == 1 | 
                        as.logical(sf::st_contains(out,unflagged)) ==TRUE))
    }
  } else {
    out <- out %>%
      dplyr::filter(cabi_verified == 1)  
  }
  
  if(isTRUE(return_df)) {
    out <- as.data.frame(sf::as_Spatial(out)) %>%
      dplyr::select(Longitude = coords.x1, Latitude = coords.x2, cabi_verified)
  }
  return(out)
}
