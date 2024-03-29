#' Downloads GBIF records of species
#'
#' Downloads GBIF records of species, removing auxiliary columns and records
#' recorded prior to a specified year, or that have coordinate uncertainty
#' above a specified amount.
#'
#' @param taxon Character. Species taxonomic name.
#' @param min_year Integer. The minimum year for which records should be
#'   collated. Default is `NULL`, i.e. no minimum.
#' @param coord_uncertainty Integer. The maximum allowable documented coordinate
#'   uncertainty (in metres). If specified, this argument will result in removal
#'   of any records that have a documented uncertainty above this limit. Records
#'   with no uncertainty documented will be returned regardless of the value of
#'   `coord_uncertainty`.
#' @param basis_of_record An optional character vector of one or more of:
#'   'FOSSIL_SPECIMEN', 'HUMAN_OBSERVATION', 'LITERATURE', 'LIVING_SPECIMEN',
#'   'MACHINE_OBSERVATION', 'MATERIAL_SAMPLE', 'OBSERVATION',
#'   'PRESERVED_SPECIMEN', or 'UNKNOWN', giving the set of allowable values for
#'   the basis of observation.
#' @param country An optional 2 letter ISO code defining the country within
#'   which occurrences should be contained. See
#'   https://www.iso.org/obp/ui/#search for valid ISO codes (see "Alpha-2 code")
#'   column.
#' @param method Either `'search'` (uses the GBIF `/occurrence/search` API
#'   endpoint), `'download'` (uses the GBIF `/occurrence/download`API endpoint),
#'   or `'auto'`, which uses `'search'` or `'download'` based on occurrence
#'   count (i.e. dataset size). The `'search'` method makes paginated queries to
#'   the API, while `'download'` performs an asynchronous query (but waits for
#'   the resulting dataset to be ready for download). The `'search'` method is
#'   limited to 100,000 records; for large datasets, consider using `'download'`
#'   or `'auto'`. When using `'download'` or `'auto'`, the arguments `username`,
#'   `pwd`, and `email` must be provided. Default is `'auto'`.
#' @param auto_threshold Integer. If `method` is `'auto'`, this argument defines
#'   the occurrence count threshold defining whether the `'search'` or
#'   `'download'` method is used. Default is `10000`, i.e. if fewer than 10000
#'   records exist for the taxon, the `'search'` method will be used. If 10000
#'   or greater records, the `'download'` method will be used. Ignored when
#'   `method` is not `'auto'`.
#' @param username  GBIF username, required when method is `'download'`.
#' @param pwd  GBIF password, required when method is `'download'`.
#' @param email Email address, required when `method = 'download'`. This _may_
#'   be used to notify user when download is ready.
#' @param retries If `method='download'` and file download fails, how many
#'   additional attempts should be made to download the file?
#' @param cleanup Logical. Should temporary files associated with
#'   `'download'` and `'auto'` method be deleted? Default is `TRUE`.
#' @details This function is a wrapper of `rgbif` such that it can be readily
#'   used with the `CoordinateCleaner` package.
#' @return A `data.frame` of species occurrence records.
#' @importFrom magrittr %>%
#' @importFrom countrycode countrycode
#' @importFrom dplyr bind_rows filter mutate
#' @importFrom httr RETRY write_disk
#' @importFrom readr col_character col_double col_integer cols_only read_delim
#' @importFrom rgbif name_backbone occ_count occ_download occ_download_wait occ_search pred pred_gte pred_in
#' @importFrom utils unzip
#' @export
get_gbif_records <- function(taxon, min_year, coord_uncertainty,
                             basis_of_record, country,
                             method=c('auto', 'search', 'download'),
                             auto_threshold=10000,
                             username, pwd, email, retries=10, cleanup=TRUE) {

  method <- match.arg(method)
  if (!missing(basis_of_record) &&
      !all(basis_of_record %in% c("FOSSIL_SPECIMEN", "HUMAN_OBSERVATION",
                                  "LITERATURE", "LIVING_SPECIMEN",
                                  "MACHINE_OBSERVATION", "MATERIAL_SAMPLE",
                                  "OBSERVATION", "PRESERVED_SPECIMEN",
                                  "UNKNOWN"))
  ) {
    stop("basis_of_record must be a vector of one or more of: ",
         "'FOSSIL_SPECIMEN', 'HUMAN_OBSERVATION', 'LITERATURE', ",
         "'LIVING_SPECIMEN', 'MACHINE_OBSERVATION', 'MATERIAL_SAMPLE', ",
         "'OBSERVATION', 'PRESERVED_SPECIMEN', 'UNKNOWN'")
  }
  if(!missing(country) && length(country) > 1) {
    stop('country must have length 1.')
  }
  if(!missing(country) &&
     !is.na(countrycode::countrycode(country, origin='iso2c', destination = 'iso2c'))) {
    stop("country not found. Check allowable values in countrycode::codelist[, c('country.name.en', 'iso2c')]")
  }
  if(method=='auto' & auto_threshold > 100000) {
    warning('auto_threshold can not be greater than 100000. ',
            'Adjusting to 100000.')
    auto_threshold <- 100000
  }
  if(method %in% c('download', 'auto') &
     any(missing(username), missing(pwd), missing(email))) {
    warning('When using method is "download" or "auto", username, pwd, and ',
            'email must be provided. Reverting to `method="search".')
    method <- 'search'
  }

  match_species <- function(sp) {
    sapply(sp, function(x) {
      species_matches <- rgbif::name_backbone(name = x)
      if(nrow(species_matches)==0) {
        stop('No matches found in GBIF for ', sp)
      }
      key <- species_matches$usageKey[1]
      message(sprintf('%s matched to %s (key: %s)', x,
                      species_matches$canonicalName[1], key))
      key
    })
  }

  key <- match_species(taxon)
  .f <- function(k, min_year, coord_uncertainty, basis_of_record, country) {

    if(method=='auto') {
      args <- list(taxonKey=k, georeferenced=TRUE)
      if(!missing(min_year)) args$from <- min_year
      if(!missing(country)) args$country <- country

      n_occ <- if(!missing(basis_of_record)) {
        sum(sapply(basis_of_record, function(b) {
          args$basisOfRecord <- b
          do.call(rgbif::occ_count, args)
        }))
      } else {
        do.call(rgbif::occ_count, args)
      }
      method <- if(n_occ < auto_threshold) 'search' else 'download'
    }
    switch(
      method,
      search={
        if(!missing(min_year)) {
          n <- rgbif::occ_count(
            taxonKey=k, georeferenced=TRUE,
            year=sprintf('%s,%s', min_year, format(Sys.Date(), '%Y'))
          )
          if(n > 100000) {
            warning(
              sprintf(
                paste('%s records exist for taxon %s.',
                      'Query limited to first %s.\n',
                      'Use method="download" to avoid this limit.'),
                n, k, 100000)
            )
          }
          dat <- rgbif::occ_search(
            taxonKey=k, limit=100000, hasCoordinate=TRUE,
            hasGeospatialIssue=FALSE,
            fields=c('key', 'scientificName', 'decimalLongitude',
                     'decimalLatitude', 'coordinateUncertaintyInMeters',
                     'year', 'countryCode'),
            year=sprintf('%s,%s', min_year, format(Sys.Date(), '%Y'))
          )$data
        } else {
          n <- rgbif::occ_count(taxonKey=k, georeferenced=TRUE)
          if(n > 100000) {
            warning(
              sprintf(
                paste('%s records exist for taxon %s.',
                      'Query limited to first %s.\n',
                      'Use method="download" to avoid this limit.'),
                n, k, 100000)
            )
          }
          args <- list(taxonKey = k, limit = 1e+05, hasCoordinate = TRUE, hasGeospatialIssue = FALSE,
                       fields = c("key", "scientificName", "decimalLongitude", "decimalLatitude",
                                  "coordinateUncertaintyInMeters", "year", "countryCode"))
          if (!missing(country)) args$country <- country
          dat <- do.call(rgbif::occ_search, args)$data
        }
        if("coordinateUncertaintyInMeters" %in% names(dat) &&
           !missing(coord_uncertainty)) {
          dat <- dat %>%
            dplyr::filter(coordinateUncertaintyInMeters <= coord_uncertainty |
                            is.na(coordinateUncertaintyInMeters))
        }
        dat
      },
      download={
        args <- list(
          rgbif::pred('taxonKey', k),
          rgbif::pred('hasCoordinate', TRUE),
          rgbif::pred('hasGeospatialIssue', FALSE),
          format='SIMPLE_CSV',
          user=username, pwd=pwd, email=email
        )
        if(!missing(min_year)) args <-
            c(list(rgbif::pred_gte('year', min_year)), args)
        if (!missing(basis_of_record)) args <- c(list(rgbif::pred_in("basisOfRecord",
                                                                     basis_of_record)), args)
        if (!missing(country)) args <- c(list(rgbif::pred_in("country", country)),
                                         args)

        dl_key <- do.call(rgbif::occ_download, args)
        message('GBIF download key: ', dl_key)
        dl <- rgbif::occ_download_wait(dl_key)
        httr::RETRY(verb = 'GET', url=dl$downloadLink, times=retries + 1,
                    quiet=FALSE, terminate_on=NULL,
                    httr::write_disk(path=f <- tempfile(), overwrite=TRUE))
        csv <- utils::unzip(f, exdir=tempfile())

        dat <- readr::read_delim(
          csv, '\t',
          col_types=readr::cols_only(
            gbifID=readr::col_character(),
            scientificName=readr::col_character(),
            decimalLongitude=readr::col_double(),
            decimalLatitude=readr::col_double(),
            coordinateUncertaintyInMeters=readr::col_double(),
            year=readr::col_integer(),
            countryCode=readr::col_character())
        )
        if(isTRUE(cleanup)) unlink(c(f, dirname(csv)), recursive=TRUE)
        if(!missing(coord_uncertainty)) {
          dat <- dat %>% dplyr::filter(coordinateUncertaintyInMeters <=
                                         coord_uncertainty |
                                         is.na(coordinateUncertaintyInMeters))
        }
        dat
      }
    )
  }
  out <- lapply(key, .f, min_year=min_year, coord_uncertainty=coord_uncertainty,
                basis_of_record=basis_of_record, country=country)

  if(is.list(out)) {
    out <-  out %>% dplyr::bind_rows()
  }

  out <- out %>%
    dplyr::filter(countryCode != "none") %>% # This causes issues with cleaning
    dplyr::mutate(countryCode = countrycode::countrycode(
      countryCode, origin =  'iso2c', destination = 'iso3c')
    )
  out <- dplyr::filter(out, !is.na(decimalLongitude), !is.na(decimalLatitude))

  return(out)
}
