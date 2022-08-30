#' Flags records that occur in countries that are not reported in either
#' infected_countries or CABI datasheet
#'
#' Cross reference occurrence records against either a vector of country names
#' with established populations specified countries, or those presented in a
#' CABI distribution file. Occurrence records that occur in countries outside
#' one of these lists are then removed.
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
#' @param return_df Logical. Return the object as a `data.frame` or as an `sf`
#'   object (i.e. `TRUE`, the default).
#' @return A `data.frame` or an `sf` points object flagging records
#'   found in countries in which do not occur in infected_countries OR
#'   is not reported as being present in CABI.
#' @importFrom magrittr %>%
#' @importFrom countrycode countrycode
#' @importFrom dplyr mutate
#' @importFrom rnaturalearth ne_countries
#' @importFrom terra buffer crds crs intersect subset vect
#' @importFrom utils read.csv
#' @export
record_flagger <- function(occurrence_records, infected_countries, cabi_ref,
                           return_df =FALSE) {

  if(missing(infected_countries) & missing(cabi_ref)) {
    stop("'infected_countries' OR a 'cabi_ref' must be supplied")
  }

  if(!missing(infected_countries) & !missing(cabi_ref)) {
    stop("'infected_countries' is supplied 'cabi_ref' will not be used")
  }

  world <- rnaturalearth::ne_countries(returnclass='sf', scale = 10) %>%
    dplyr::mutate(iso_n3 = as.integer(as.numeric(iso_n3))) %>%
    terra::vect()

  if(is.character(occurrence_records)) {
    occurrence_records <- utils::read.csv(occurrence_records)
  }

  occ <- suppressMessages(
    terra::vect(occurrence_records, geom = c('Longitude', 'Latitude'),
                crs = terra::crs(world))
  )

  # Preference infected_country argument over cabi_ref
  # User can supply either... but only one will be used
  if(!missing(infected_countries)) {

    infected <- countrycode::countrycode(infected_countries,
                                         'country.name', 'iso3n')

    # Get infected_country polygons
    country_polygons <- world %>%
      terra::subset(world$iso_n3 %in% unique(infected))

  } else {
    cabi <- suppressMessages(terra::vect(read.csv(file = cabi_ref,skip = 2),
                                         geom = c('Longitude', 'Latitude'),
                                         crs = terra::crs(world)))

    # Get CABI country names
    cabi_countries <- suppressMessages(
      suppressWarnings(terra::intersect(cabi, terra::buffer(world, 0))$name)
    ) %>% unique

    # Get CABI country polygons
    country_polygons <- world %>%
      terra::subset(world$name %in% cabi_countries)
  }

  # Identify points that are present within CABI identified countries
  out <- suppressMessages(suppressWarnings(
    terra::intersect(occ, country_polygons)
  ))

  if(isTRUE(return_df)) {
    out <- terra::crds(out) %>%
      as.data.frame() %>%
      setNames(c('Longitude', 'Latitude'))
  }
  return(out)
}
