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
#' @param country An optional vector of one or more 2-letter codes defining the
#'   set of countries to which returned occurrences should be restricted. See
#'   `[edmaps::countries]` for valid country codes.
#' @param method Either `'search'` (uses the GBIF `/occurrence/search` API
#'   endpoint), `'download'` (uses the GBIF `/occurrence/download`API endpoint),
#'   or `'auto'`, which uses `'search'` or `'download'` based on occurrence
#'   count (i.e. dataset size). The `'search'` method makes paginated queries to
#'   the API, while `'download'` performs an asynchronous query (but waits for
#'   the resulting dataset to be ready for download). The `'search'` method is
#'   limited to 100,000 records; for large datasets, consider using `'download'`
#'   or `'auto'`. When using `'download'` or `'auto'`, the arguments `username`,
#'   `pwd`, and `email` must be provided. Default is `'auto'`. Note that when
#'   using `'auto'`, the estimated dataset size max overestimate the true size,
#'   due to the underlying estimation methods not taking all variables into
#'   account (e.g. does not account for the `coord_uncertainty` constraint).
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
#' @param cleanup Logical. Should temporary files associated with
#'   `'download'` and `'auto'` method be deleted? Default is `TRUE`.
#' @details This function is a wrapper of `rgbif` such that it can be readily
#'   used with the `CoordinateCleaner` package.
#' @return A `data.frame` of species occurrence records.
#' @importFrom magrittr %>%
#' @importFrom cli cli_alert_info
#' @importFrom countrycode countrycode
#' @importFrom dplyr any_of bind_rows filter mutate select
#' @importFrom rgbif name_backbone occ_count occ_data occ_download occ_download_get occ_download_import occ_download_wait pred pred_gte pred_in pred_isnull pred_lte pred_or
#' @export
get_gbif_records <- function(taxon, min_year, coord_uncertainty,
                              basis_of_record, country,
                              method=c('auto', 'search', 'download'),
                              auto_threshold=10000,
                              username, pwd, email, cleanup=TRUE) {

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
  if(!missing(country)) {
    invalid_country <- setdiff(country, edmaps::countries$gbif2c)
    if(length(invalid_country) > 0) {
      stop(sprintf(
        'Invalid country: %s.\nSee `edmaps::countries` for country codes recognised by the GBIF API.',
        paste(invalid_country, collapse=', ')
      ), call.=FALSE)
    }
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
      species_matches <- rgbif::name_backbone(name = x, curlopts=list(http_version=2))
      if(nrow(species_matches)==0) {
        stop('No matches found in GBIF for ', sp)
      }
      key <- species_matches$usageKey[1]
      cli::cli_alert_info(
        "{x} matched to {.href [{species_matches$canonicalName[1]}](https://www.gbif.org/species/{key})} (key: {key})."
      )

      key
    })
  }

  key <- match_species(taxon)

  if(method != 'download') {
    args <- list(taxonKey=key)
    if(!missing(min_year)) args$from <- min_year
    if(!missing(country)) args$country <- country
    if(!missing(basis_of_record)) args$basisOfRecord <- basis_of_record
    args <- expand.grid(args, stringsAsFactors=FALSE) %>%
      dplyr::mutate(georeferenced=TRUE, curlopts=list(http_version=2)) %>%
      split(seq_len(nrow(.)))
    n_occ <- sum(sapply(args, function(x) {
      do.call(rgbif::occ_count, x)
    }))
    if(method == 'auto') {
      method <- if(n_occ > auto_threshold) 'download' else 'search'
    }
    if(method == 'search' && n_occ > 100000) {
      warning(sprintf(paste(
        '%s records exist for taxon %s. Query limited to first 100,000.\n',
        'Use method="download" or method="auto" to avoid this limit.'
      ), n_occ, paste(key, collapse='/')), call.=FALSE)
    }
  }

  occ <- switch(
    method,
    search={
      args <- list(
        taxonKey=paste(key, collapse=';'), limit=100000, hasCoordinate=TRUE,
        hasGeospatialIssue=FALSE, curlopts=list(http_version=2)
      )
      if(!missing(min_year))
        args$year <- sprintf('%s,%s', min_year, format(Sys.Date(), '%Y'))
      if(!missing(country)) args$country <- country
      if(!missing(basis_of_record)) args$basisOfRecord <- basisOfRecord
      occ <- do.call(rgbif::occ_data, args)
      # occ_search has a coordinateUncertaintyInMeters arg but not possible to
      # include null coordinateUncertaintyInMeters via that arg.
      if("coordinateUncertaintyInMeters" %in% names(occ) &&
         !missing(coord_uncertainty)) {
        occ <- occ %>%
          dplyr::filter(
            coordinateUncertaintyInMeters <= coord_uncertainty |
              is.na(coordinateUncertaintyInMeters)
          )
      }
      switch(attr(occ, 'type'),
             single=occ$data,
             many=dplyr::bind_rows(lapply(occ, '[[', 'data'))
      ) %>% dplyr::select(dplyr::any_of(c(
        'key', 'scientificName', 'decimalLongitude',
        'decimalLatitude', 'coordinateUncertaintyInMeters',
        'year', 'countryCode'
      )))
    },
    download={
      args <- list(
        rgbif::pred_in('taxonKey', key),
        rgbif::pred('hasCoordinate', TRUE),
        rgbif::pred('hasGeospatialIssue', FALSE),
        format='SIMPLE_CSV',
        user=username, pwd=pwd, email=email,
        curlopts=list(http_version=2)
      )
      if(!missing(min_year)) {
        args <- append(args, list(rgbif::pred_gte('year', min_year)))
      }
      if(!missing(basis_of_record)) {
        args <- append(args, list(rgbif::pred_in("basisOfRecord", basis_of_record)))
      }
      if(!missing(country)) {
        args <- append(args, list(rgbif::pred_in("country", country)))
      }
      if(!missing(coord_uncertainty)) {
        args <- append(
          args,
          list(rgbif::pred_or(
            rgbif::pred_lte('coordinateUncertaintyInMeters', coord_uncertainty),
            rgbif::pred_isnull('coordinateUncertaintyInMeters')
          ))
        )
      }
      dl_key <- do.call(rgbif::occ_download, args)
      message('GBIF download key: ', dl_key)
      dl <- rgbif::occ_download_wait(dl_key, curlopts=list(http_version=2))
      f <- rgbif::occ_download_get(dl_key, tempdir(), overwrite=TRUE,
                                   http_version=2)
      occ <- f %>%
        rgbif::occ_download_import(
          select=c('gbifID', 'scientificName', 'decimalLongitude',
                   'decimalLatitude', 'coordinateUncertaintyInMeters', 'year',
                   'countryCode'),
          quote=""
        )
      if(isTRUE(cleanup)) {
        unlink(f)
        unlink(
          file.path(tempdir(), 'gbifdownload', sub('\\.zip', '', basename(f))),
          recursive=TRUE
        )
      }
      occ
    })

  occ <- dplyr::mutate(occ, iso3c=countrycode::countrycode(
    countryCode, origin='iso2c', destination='iso3c')
  )
  occ <- dplyr::filter(occ, !is.na(decimalLongitude), !is.na(decimalLatitude))

  return(occ)
}
