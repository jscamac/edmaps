#' Country codes recognised by the GBIF API
#'
#' A data.frame comprising country codes recognised by the GBIF API, and their
#' associated country names.
#'
#' @format ## `countries`
#' A data frame with 252 rows and 2 columns:
#' \describe{
#'   \item{gbif2c}{GBIF country code}
#'   \item{name}{Country name}
#' }
#' @details Occurrence data held by GBIF have associated metadata that includes
#'   the name of the country within which the occurrence was observed. Queries
#'   to the GBIF API can be filtered to return occurrences from specific
#'   countries, by passing country codes to the query. Country codes recognised
#'   by GBIF are roughly equal to those included in ISO-3166-1, yet there are
#'   some differences. This dataset includes the full set of accepted country
#'   codes, as at the time of package compilation.
#' @source <https://raw.githubusercontent.com/gbif/portal16/master/locales/source/en-DK/enums/country.json>
'countries'
