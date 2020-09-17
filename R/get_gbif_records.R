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
#' @param method Either `'search'` (uses the GBIF `/occurrence/search` API
#'   endpoint) or `'download'` (uses the GBIF `/occurrence/download`API
#'   endpoint). The former makes paginated queries to the API, while the latter
#'   performs an asynchronous query (but waits for the resulting dataset to be
#'   ready for download). The `'search'` method is limited to 100,000 records;
#'   for large datasets, consider using `'download'`. When using 
#'   `method='download'`, the arguments `username`, `pwd`, and `email` must be 
#'   provided.
#' @param username  GBIF username, required when method is `'download'`.
#' @param pwd  GBIF password, required when method is `'download'`.
#' @param username  An email address for GBIF notification when download is 
#'   ready, required when method is `'download'`.
#' @param email Email address, required when `method = 'download'`. This _may_ 
#'   be used to notify user when download is ready.
#' @details This function is a wrapper of `rgbif` such that it can be readily
#'   used with the `CoordinateCleaner` package.
#' @return A `data.frame` of species occurrence records.
#' @importFrom magrittr %>%
#' @importFrom rgbif name_suggest pred pred_gte occ_download occ_search occ_download_wait
#' @importFrom dplyr bind_rows filter mutate select
#' @importFrom countrycode countrycode
#' @importFrom readr read_delim cols_only col_character col_double col_integer
#' @importFrom utils download.file unzip
#' @export
get_gbif_records <- function(taxon, min_year, coord_uncertainty, 
                             method=c('search', 'download'), username, pwd, 
                             email) {
  # warning about 100k limit
  method <- match.arg(method)
  if(method=='download' & any(missing(username), missing(pwd), missing(email))) {
    warning('When using `method="download"`, username, pwd, and email must ',
            'be provided. Reverting to `method="search".')
    method <- 'search'
  }
  
  match_species <- function(sp) {
    sapply(sp, function(x) {
      species_matches <- rgbif::name_suggest(q=x)$data
      if(nrow(species_matches)==0) {
        stop('No matches found in GBIF for ', sp)
      }
      key <- species_matches$key[1]
      message(sprintf('%s matched to %s (key: %s)', x, 
                      species_matches$canonicalName[1], key))    
      key
    })
  }
  
  key <- match_species(taxon)
  .f <- function(k, min_year, coord_uncertainty) {
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
          dat <- rgbif::occ_search(
            taxonKey=k, limit=100000, hasCoordinate=TRUE,
            hasGeospatialIssue=FALSE,
            fields=c('key', 'scientificName', 'decimalLongitude', 
                     'decimalLatitude', 'coordinateUncertaintyInMeters',
                     'year', 'countryCode')
          )$data
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
          user=username, pwd=pwd, email=email
        )
        if(!missing(min_year)) args <- 
            c(list(rgbif::pred_gte('year', min_year)), args)
        
        dl_key <- do.call(rgbif::occ_download, args)
        message('GBIF download key: ', dl_key)
        dl <- rgbif::occ_download_wait(dl_key)
        utils::download.file(dl$downloadLink, destfile=f <- tempfile())
        utils::unzip(f, exdir=d <- tempfile())
        
        dat <- readr::read_delim(
          f, '\t', col_types=readr::cols_only(
            gbifID=readr::col_character(), 
            scientificName=readr::col_character(), 
            decimalLongitude=readr::col_double(), 
            decimalLatitude=readr::col_double(), 
            coordinateUncertaintyInMeters=readr::col_double(), 
            year=readr::col_integer(), 
            countryCode=readr::col_character())
        )
        if(!missing(coord_uncertainty)) {
          dat <- dat %>% dplyr::filter(coordinateUncertaintyInMeters <= 
                                     coord_uncertainty |
                                     is.na(coordinateUncertaintyInMeters))
        }
        dat
      }
    )
  }
  out <- lapply(key, .f, min_year=min_year, coord_uncertainty=coord_uncertainty)
  
  if(is.list(out)) {
    out <-  out %>% dplyr::bind_rows()
  }
  
  out <- out %>%
    dplyr::filter(countryCode != "none") %>% # This causes issues with cleaning
    dplyr::mutate(countryCode = countrycode::countrycode(
      countryCode, origin =  'iso2c', destination = 'iso3c')
    )
  
  return(out)
}

