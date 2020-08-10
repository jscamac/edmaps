#' Flags records that occur in countries that are not reported in either
#' infected_countries or CABI datasheet
#'
#' `record_flagger` cross references occurrence records against either a vector
#' of country names with established populations specified in `infected_countries` 
#' or those presented in a CABI distribution file. It will then either flag 
#' or automatically remove occurrence records that occur in countries outside 
#' one of these lists.
#' 
#' @param occurrence_records A `data.frame`, `sf` object or `SpatialPoints*`
#'   object, or a path to a .csv file containing the locations of species of
#'   interest. (If a `data.frame` is provided, it must contain the named columns
#'   "Latitude" and "Longitude").
#' @param infected_countries Character vector of countries with known established
#' populations.   
#' @param cabi_ref Character. The path to a .csv file downloaded from a CABI
#'   datasheet containing the country-scale distributional data for the pest.
#'   Note that if infected_countries the CABI data will not be used.
#' @param manual_check Logical. Allows interactive selection of which 
#'   unverified points to retain. Interactive map will only be produced if 
#'   unverified records are present and if this argument is set to `TRUE`.
#' @param return_df Logical. Return the object as a `data.frame` or as an `sf`
#'   object (i.e. `TRUE`, the default).
#' @return A `data.frame` or an `sf` points object flagging records 
#'   found in countries in which do not occur in infected_countries OR
#'   is not reported as being present in CABI.
#' @importFrom magrittr "%>%"
#' @importFrom rnaturalearth ne_countries
#' @importFrom utils read.csv
#' @importFrom sf st_as_sf st_crs st_intersection st_contains as_Spatial
#' @importFrom dplyr filter mutate select
#' @importFrom mapedit selectFeatures
#' @importFrom countrycode countrycode
#' @export

record_flagger <- function(occurrence_records, 
                           infected_countries, 
                           cabi_ref, 
                           manual_check = FALSE,
                           return_df =FALSE) {
  
  if(missing(infected_countries) & missing(cabi_ref)) {
    stop("'infected_countries' OR a 'cabi_ref' must be supplied")
  }
  
  if(!missing(infected_countries) & !missing(cabi_ref)) {
    stop("'infected_countries' is supplied 'cabi_ref' will not be used")
  }
  
  world <- rnaturalearth::ne_countries(returnclass='sf',scale = 10) %>%
    dplyr::mutate(iso_n3 = as.integer(as.numeric(iso_n3)))
  
  if(is.character(occurrence_records)) {
    occurrence_records <- utils::read.csv(occurrence_records)
  }
  
  occ <- suppressMessages(sf::st_as_sf(occurrence_records,
                                       coords = c("Longitude","Latitude"),
                                       crs = sf::st_crs(world)))
  
  # Preference infected_country argument over cabi_ref
  # User can supply either... but only one will be used
  if(!missing(infected_countries)) {
    
    infected <- countrycode::countrycode(infected_countries, 
                                         'country.name', 'iso3n')
    
    # Get infected_country polygons
    country_polygons <-  world %>%
      dplyr::filter(iso_n3 %in% unique(infected))
    
  } else {
  cabi <- suppressMessages(sf::st_as_sf(read.csv(file = cabi_ref,skip = 2),
                                        coords = c("Longitude","Latitude"),
                                        crs = sf::st_crs(world)))
  
  # Get CABI country names
  cabi_countries <- suppressMessages(
    suppressWarnings(sf::st_intersection(cabi, sf::st_buffer(world, 0))$name)
  )
  
  # Get CABI country polygons
  country_polygons <- world %>%
    dplyr::filter(name %in% unique(cabi_countries))
  }
  
  # Identify points that are present within CABI identified countries
  out <- suppressMessages(suppressWarnings(
    sf::st_join(occ, country_polygons, join = sf::st_intersects)
  )) %>%
    dplyr::mutate(verified = as.numeric(!is.na(name))) %>%
    dplyr::select(verified)
  
  
  # If unverified CABI records... check
  if(manual_check == TRUE) {
    
    if(sum(out$verified ==0) > 0) {
      
      message("Please select points to retain")
      unflagged <- suppressMessages(
        out %>%
          dplyr::filter(verified == 0) %>%
          mapedit::selectFeatures(
            ., title = "Select unverified records to retain")
      )
      
      out <- suppressMessages(
        out %>%
          dplyr::filter(verified == 1 | 
                          as.logical(sf::st_contains(out,unflagged)) ==TRUE)
      )
    }
  } else {
    out <- out %>%
      dplyr::filter(verified == 1)  
  }
  
  if(isTRUE(return_df)) {
    out <- as.data.frame(sf::as_Spatial(out)) %>%
      dplyr::select(Longitude = coords.x1, Latitude = coords.x2, verified)
  }
  return(out)
}
