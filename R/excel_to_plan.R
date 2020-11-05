#' Create a drake plan using tabular species data
#'
#' Import parameters from an Excel xlsx file and create a drake plan to 
#' estimate establishment likelihoods.
#' 
#' @param file Character. File path to an xlsx file containing required 
#'   parameters for creating a species plan. See Details and 
#'   \code{\link{species_plan}} for further information.
#' @return A drake plan with a combined workflow for all species.
#' @details To simplify reproducibility, \code{edmaps} provides an 
#'   \emph{Excel} interface for specifying species parameters relevant to
#'   estimating establishment likelihood. An example spreadsheet is 
#'   bundled with the package, available at the path given by 
#'   \code{system.file('extdata/parameters.xlsx', package='edmaps')}. The 
#'   spreadsheet has two sheets, the first specifying "global" parameters
#'   that will apply to all species (e.g. file paths to rasters that will),
#'   be used regardless of species identity and the second specifying
#'   parameters that can vary by species. In the second sheet, each row
#'   corresponds to a separate species. Tooltips and data validation
#'   guide the user with respect to expected/allowable data.
#' @importFrom readxl read_excel
#' @importFrom dplyr select rename mutate bind_rows
#' @importFrom tidyr spread
#' @importFrom magrittr '%>%'
#' @export
excel_to_plan <- function(file) {
  globals <- readxl::read_excel(file, sheet='Global parameters', skip=1,
                                col_types=c('text', 'skip', 'list')) %>% 
    tidyr::spread(Variable, Value) %>% 
    # standardise variable names
    dplyr::select(make_interactive_maps=`Make interactive maps?`,
                  clum_path=`CLUM raster path`,
                  nvis_path=`NVIS raster path`,
                  ndvi_path=`NDVI raster path`,
                  postcode_path=`Postcode shapefile path`,
                  containers_data_path=`Containers data path`,
                  port_data_path=`Marine ports data path`,
                  fertiliser_data_path=`Fertiliser data path`,
                  nrm_path=`NRM shapefile path`,
                  airport_beta=`Airport distance penalty (tourists)`,
                  airport_tsi_beta=`Airport distance penalty (Torres)`,
                  total_tourists=`Total tourists`,
                  total_returning=`Total returning residents`,
                  total_torres=`Total passengers from TSI`,
                  total_mail=`Total mail`,
                  total_vessels=`Total vessels`,
                  total_fertiliser=`Total fertiliser`,
                  total_machinery=`Total machinery`,
                  total_nurserystock=`Total nurserystock`,
                  total_goods=`Total goods`) %>% 
    lapply(unlist)
  
  # Check that supplied file paths exist
  paths_global <- na.omit(unlist(globals[grep('_path$', names(globals))]))
  if(length(paths_global) > 0 && !all(file.exists(paths_global))) {
    stop('File not found:\n    - ', 
         paste0(paths_global[!file.exists(paths_global)], collapse='\n    - '))
  }
  
  species <- readxl::read_excel(file, sheet='Species-specific parameters') %>% 
    # standardise names
    dplyr::rename(species=Species,
                  pathways=Pathways,
                  include_abiotic_weight=`Include abiotic weight?`,
                  include_ndvi=`Include NDVI?`,
                  include_nvis=`Include NVIS?`,
                  clum_classes=`CLUM classes`,
                  nvis_classes=`NVIS classes`,
                  gbif_species=`GBIF species name(s)`,
                  gbif_min_year=`GBIF min year`,
                  occurrence_path=`Occurrence path`,
                  infected_countries=`Infected countries`,
                  climate_suitability_path=`Climate suitability path`,
                  exclude_bioclim_vars =`Exclude BIOCLIM vars`,
                  prob_tourists=`Prob tourists`,
                  prob_returning=`Prob returning resident`,
                  prob_torres=`Prob Torres passenger`,
                  prob_mail=`Prob mail`,
                  prob_vessels=`Prob vessels`,
                  port_weight_beta=`Distance penalty (ports)`,
                  prob_fertiliser=`Prob fertiliser`,
                  prob_machinery=`Prob machinery`,
                  prob_containers=`Prob containers`,
                  prob_nurserystock=`Prob nurserystock`,
                  prob_goods=`Prob goods`,
                  aggregated_res=`Aggregated res`) %>% 
    dplyr::mutate(
      species=gsub('^\\s+|\\s+$', '', species),
      species=gsub('\\s+', ' ', species),
      clum_classes=expand_range(strsplit(
        gsub('^\\s+|\\s+$', '', clum_classes), '\\s*,\\s*')),
      nvis_classes=expand_range(strsplit(
        gsub('^\\s+|\\s+$', '', nvis_classes), '\\s*,\\s*')),
      pathways=strsplit(gsub('^\\s+|\\s+$', '', pathways), '\\s*,\\s*'),
      exclude_bioclim_vars=strsplit(
        gsub('^\\s+|\\s+$', '', exclude_bioclim_vars), '\\s*,\\s*'),
      infected_countries=gsub('^\\s+|\\s+$', '', infected_countries)
    )
  
  paths_species <- species %>% 
    select(grep('_path$', colnames(.))) %>% 
    unlist() %>% 
    setdiff(NA)
  
  if(length(paths_species) > 0 && !all(file.exists(paths_species))) {
    stop('File not found:\n    - ', 
         paste0(paths_species[!file.exists(paths_species)], collapse='\n    - '))
  }
  
  invalid_abiotic <- species$include_abiotic_weight & 
    (is.na(species$occurrence_path) & 
       is.na(species$gbif_species) & 
       is.na(species$climate_suitability_path))
  
  if(any(invalid_abiotic)) {
    # ^ if abiotic weight is used, then if no occurrence/gbif data are used and 
    #   climate suitability path not provided, stop.
    stop('If include_abiotic_weight is TRUE, either climate_suitability_path ',
         'or else gbif_species and/or occurrence_path must be provided.')
  }
  
  cabi_paths_idx <- which(grepl('\\.csv$', species$infected_countries) & 
         species$include_abiotic_weight)
  exist <- file.exists(species$infected_countries[cabi_paths_idx])
  
  if(any(!exist)) {
    stop('File not found:\n    - ', 
         paste0(species$infected_countries[cabi_paths_idx[!exist]], collapse='\n    - ')) 
  }
  species$cabi_path <- NA
  species$cabi_path[cabi_paths_idx] <- species$infected_countries[cabi_paths_idx]
  species$infected_countries[cabi_paths_idx] <- NA
  
  # Test if any countries are invalid
  if(any(!is.na(species$infected_countries))) {
    countries <- unique(unlist(strsplit(setdiff(species$infected_countries, NA), 
                                 '\\s*,\\s*')))
    testmatch <- countrycode::countrycode(
      countries, 'country.name', 'iso3n', warn=FALSE)
    
    if(any(is.na(testmatch))) {
      stop('Unrecognised countries:\n    - ',
           paste0(countries[is.na(testmatch)],
                  collapse='\n    - '),
           '\nSee countrycode::codelist$country.name.en for allowed values.')
    }
  }
  
  n <- nrow(species)
  plans <- lapply(seq_len(n), function(i) {
    x <- as.list(species[i, ])
    x <- x[!sapply(x, is.na)]
    x[sapply(x, is.list)] <- lapply(x[sapply(x, is.list)], unlist)
    
    if('gbif_species' %in% names(x)) {
      if(!'use_gbif' %in% names(x) || x$use_gbif) {
        x$gbif_species <- gsub('^\\s+|\\s+$', '', x$gbif_species)
        x$use_gbif <- !is.na(x$gbif_species) & (nchar(x$gbif_species) > 0)
        x$gbif_species <- strsplit(x$gbif_species, '\\s*,\\s*')[[1]]
      }
    }
    
    if('infected_countries' %in% names(x)) {
      x$infected_countries <- gsub('^\\s+|\\s+$', '', x$infected_countries)
      x$infected_countries <- strsplit(x$infected_countries, '\\s*,\\s*')[[1]]
    }
    
    if(length(x$clum_classes)==0) x$clum_classes <- NULL
    if(length(x$nvis_classes)==0) x$nvis_classes <- NULL
    args <-  c(
      x[setdiff(names(x), 'include_nvis')], 
      globals[intersect(names(globals), names(formals(species_plan)))])
    do.call(species_plan, args)
  })
  return(unique(dplyr::bind_rows(plans)))
}
