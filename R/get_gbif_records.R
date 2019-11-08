#' Downloads GBIF records of species
#'
#' Downloads GBIF records of species, removing auxiliary columns and records 
#' recorded prior to a specified year, or that have coordinate uncertainty
#' above a specified amount.
#' 
#' @param species Character. Species taxonomic name.
#' @param limit Integer. Number of records to return. Max \code{limit} is
#'   \code{200000}.
#' @param min_year Integer. The minimum year for which records should be
#'   collated. Default is \code{NULL}, i.e. no minimum.
#' @param coord_uncertainty Integer. The maximum allowable documented 
#'   coordinate uncertainty (in metres). If specified, this argument will 
#'   result in removal of any records that have a documented uncertainty above 
#'   this limit. Records with no uncertainty documented will be returned 
#'   regardless of the value of \code{coord_uncertainty}.
#' @details This function is a wrapper of \code{rgbif} such that it can be
#'   readily used with \code{CoordinateCleaner} package.
#' @return A \code{data.frame} of species occurrence records.
#' @importFrom rgbif occ_search
#' @importFrom dplyr bind_rows filter mutate select
#' @importFrom countrycode countrycode
#' @export
get_gbif_records <- function(species, limit = 200000, min_year, 
  coord_uncertainty) {

  out <- rgbif::occ_search(scientificName = species, limit = limit, 
    return = "data", hasCoordinate = TRUE)
  
  if(is.list(out)) {
    out <- out %>% 
    dplyr::bind_rows()
  }
  
  out <- out %>%
  dplyr::filter(countryCode!="none") %>% # This causes issues with cleaning
  dplyr::mutate(countryCode = countrycode::countrycode(
    countryCode, origin =  'iso2c', destination = 'iso3c')
  )
  
  if(!missing(min_year)) {
    out <- out %>%
    dplyr::filter(year >= min_year)
  }
  
  if("coordinateUncertaintyInMeters" %in% names(out)) {
    if(!missing(coord_uncertainty)) {
      out <- out %>%
      dplyr::filter(coordinateUncertaintyInMeters <= coord_uncertainty |
                      is.na(coordinateUncertaintyInMeters))
    }
  }
  return(out)
}
