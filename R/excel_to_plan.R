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
#' @importFrom drake file_in file_out drake_plan
#' @importFrom raster stack unstack writeRaster
#' @importFrom countrycode countrycode
#' @importFrom magrittr '%>%'
#' @export
excel_to_plan <- function(file) {
  globals <- readxl::read_excel(file, sheet='Global parameters', skip=1,
                                col_types=c('text', 'skip', 'list')) %>%
    tidyr::spread(Variable, Value) %>%
    # standardise variable names
    dplyr::select(make_interactive_maps=`Make interactive maps?`,
                  gbif_username=`GBIF username`,
                  gbif_password=`GBIF password`,
                  basemap_mode=`Basemap mode`,
                  minimum_probability_for_maps=`Minimum probability threshold`,
                  clum_path=`CLUM raster path`,
                  nvis_path=`NVIS raster path`,
                  ndvi_path=`NDVI raster path`,
                  postcode_path=`Postcode shapefile path`,
                  containers_data_path=`Containers data path`,
                  port_data_path=`Marine ports data path`,
                  fertiliser_data_path=`Fertiliser data path`,
                  nrm_path=`NRM shapefile path`,
                  airport_beta=`Airport distance penalty (tourists)`,
                  airport_tsi_beta=`Airport distance penalty (Torres)`) %>%
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
                  species_group=`Species group`,
                  pathways=Pathways,
                  include_abiotic_weight=`Include abiotic weight?`,
                  include_ndvi=`Include NDVI?`,
                  include_nvis=`Include NVIS?`,
                  host_path=`Host distribution`,
                  clum_classes=`CLUM classes`,
                  nvis_classes=`NVIS classes`,
                  gbif_species=`GBIF species name(s)`,
                  gbif_min_year=`GBIF min year`,
                  occurrence_path=`Occurrence path`,
                  infected_countries=`Infected countries`,
                  climate_suitability_path=`Climate suitability path`,
                  exclude_bioclim_vars =`Exclude BIOCLIM vars`,
                  port_weight_beta=`Distance penalty (ports)`,
                  leakage_tourists=`Tourist leakage`,
                  establishment_tourists=`Tourist establishment`,
                  leakage_returning=`Returning resident leakage`,
                  establishment_returning=`Returning resident establishment`,
                  leakage_torres=`Torres passenger leakage`,
                  establishment_torres=`Torres passenger establishment`,
                  leakage_mail=`Mail leakage`,
                  establishment_mail=`Mail establishment`,
                  leakage_vessels=`Vessels leakage`,
                  establishment_vessels=`Vessels establishment`,
                  leakage_fertiliser=`Fertiliser leakage`,
                  establishment_fertiliser=`Fertiliser establishment`,
                  leakage_machinery=`Machinery leakage`,
                  establishment_machinery=`Machinery establishment`,
                  leakage_containers=`Containers leakage`,
                  establishment_containers=`Containers establishment`,
                  leakage_nurserystock=`Nurserystock leakage`,
                  establishment_nurserystock=`Nurserystock establishment`,
                  leakage_food=`Food leakage`,
                  establishment_food=`Food establishment`,
                  leakage_goods=`Goods leakage`,
                  establishment_goods=`Goods establishment`,
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
    dplyr::select(grep('_path$', colnames(.))) %>%
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

    # split probabilities and coerce to numeric vector
    x[grep('^leakage_|^establishment_', names(x))] <-
      lapply(x[grep('^leakage_|^establishment_', names(x))], function(y) {
        as.numeric(strsplit(as.character(y), '\\s*,\\s*')[[1]])
      })
    prob_lens <- lengths(x[grep('^leakage_|^establishment_', names(x))])
    if(any(prob_lens != 2)) {
      stop('Leakage and establishment rates must be given as a pair of ',
           'bounding values, defining the bounds of a 95% CI.\nValues should ',
           'be given as a comma-separated text string, e.g. "1,10".')
    }
    invalid_establishment <-
      sapply(x[grep('^establishment_', names(x))], min) < 0 |
      sapply(x[grep('^establishment_', names(x))], max) > 1
    if(any(invalid_establishment)) {
      stop('Establishment rates must be between 0 and 1.')
    }
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
    x$gbif_username <- sub('^\\s+$', '', globals$gbif_username)
    x$gbif_password <- sub('^\\s+$', '', globals$gbif_password)
    args <-  c(
      x[setdiff(names(x), c('include_nvis', 'gbif_username', 'gbif_password',
                            'species_group'))],
      globals[intersect(names(globals), names(formals(species_plan)))])
    if(is.na(args$gbif_username)) args$gbif_username <- NULL
    if(is.na(args$gbif_password)) args$gbif_password <- NULL
    if(is.na(args$basemap_mode)) args$basemap_mode <- 'osm'
    do.call(species_plan, args)
  })

  # Group species if necessary
  groups <- species[!is.na(gsub('^\\s+|\\s+$', '', species$species_group)),
                    c('species', 'species_group')]
  if(nrow(groups > 0)) {
    plans$group_plans <- lapply(unique(groups$species_group), function(x) {
      group <- gsub('\\s+', '_', x)
      species_in_group <- groups$species[groups$species_group==x]
      ff <- sprintf('outputs/%1$s/%1$s_edmap_%2$s.tif',
                    gsub(' ', '_', species_in_group), 1000)
      f_out <- sprintf('outputs/%1$s/%1$s_group_edmap_%2$s.tif',
                       gsub(' ', '_', group), 1000)
      res <- c(1000, 1000) # enforce 1km for now - memory safe
      aggregated_res <- c(5000, 5000) # hardcoding for now (for grouped species' maps)
      agg_factor <- aggregated_res[1]/res[1]
      group_plan <- drake::drake_plan(
        group_establishment_likelihood = {
          s <- raster::stack(drake::file_in(!!ff))
          r <- Reduce(function(x, y) prod(x, y, na.rm=TRUE), raster::unstack(1 - s))
          if(!dir.exists(dirname(!!f_out))) dir.create(dirname(!!f_out))
          raster::writeRaster(1 - r, drake::file_out(!!f_out))
        },
        plot_group_national_establishment_likelihood = static_map(
          ras = drake::file_in(
            !!sprintf("outputs/%s/%s_group_edmap_%s.tif", group, group, res[1])
          ),
          xlim = c(112.76, 155),
          ylim = c(-44.03, -9.21),
          legend_title = !!sprintf("log10(EL %skm)", round(aggregated_res[1]/1000, 2)),
          set_value_range = c(!!globals$minimum_probability_for_maps, Inf),
          scale_type = "log10",
          transparency = 1,
          aggregate_raster = list(!!agg_factor, max),
          height = 7,
          nrow = 1,
          outfile = drake::file_out(
            !!sprintf("outputs/%s/static_maps/%s_group_edmap_national_%s.pdf",
                    group, group, aggregated_res[1])
          )
        ),

        group_cairns_edmap = static_map(
          ras = drake::file_in(!!sprintf(
            "outputs/%s/%s_group_edmap_%s.tif", group, group, res[1])
          ),
          xlim = c(145.23, 146.14),
          ylim = c(-17.34, -16.68),
          legend_title = !!sprintf("log10(EL %skm)", round(res[1]/1000, 2)),
          set_value_range = c(!!globals$minimum_probability_for_maps, Inf),
          scale_type = "log10",
          transparency = 0.7,
          colramp_entire_range = TRUE,
          height = 7,
          nrow = 1,
          outfile = drake::file_out(
            !!sprintf("outputs/%s/static_maps/%s_group_edmap_cairns_%s.pdf", group, group, res[1])
          )
        ),

        group_brisbane_edmap = static_map(
          ras = drake::file_in(!!sprintf(
            "outputs/%s/%s_group_edmap_%s.tif", group, group, res[1])
          ),
          xlim = c(151.76, 153.86),
          ylim = c(-28.11, -26.45),
          legend_title = !!sprintf("log10(EL %skm)", round(res[1]/1000, 2)),
          set_value_range = c(!!globals$minimum_probability_for_maps, Inf),
          scale_type = "log10",
          transparency = 0.7,
          colramp_entire_range = TRUE,
          height = 7,
          nrow = 1,
          outfile = drake::file_out(
            !!sprintf("outputs/%s/static_maps/%s_group_edmap_brisbane_%s.pdf", group, group, res[1])
          )
        ),

        group_sydney_edmap = static_map(
          ras = drake::file_in(!!sprintf(
            "outputs/%s/%s_group_edmap_%s.tif", group, group, res[1])
          ),
          xlim = c(150.29, 151.5),
          ylim = c(-34.25, -33.51),
          legend_title = !!sprintf("log10(EL %skm)", round(res[1]/1000, 2)),
          set_value_range = c(!!globals$minimum_probability_for_maps, Inf),
          scale_type = "log10",
          transparency = 0.7,
          colramp_entire_range = TRUE,
          height = 7,
          nrow = 1,
          outfile = drake::file_out(
            !!sprintf("outputs/%s/static_maps/%s_group_edmap_sydney_%s.pdf", group, group, res[1])
          )
        ),

        group_melbourne_edmap = static_map(
          ras = drake::file_in(!!sprintf(
            "outputs/%s/%s_group_edmap_%s.tif", group, group, res[1])
          ),
          xlim = c(144.5, 145.5),
          ylim = c(-38.1, -37.5),
          legend_title = !!sprintf("log10(EL %skm)", round(res[1]/1000, 2)),
          set_value_range = c(!!globals$minimum_probability_for_maps, Inf),
          scale_type = "log10",
          transparency = 0.7,
          colramp_entire_range = TRUE,
          height = 7,
          nrow = 1,
          outfile = drake::file_out(
            !!sprintf("outputs/%s/static_maps/%s_group_edmap_melbourne_%s.pdf", group, group, res[1])
          )
        ),

        group_hobart_edmap = static_map(
          ras = drake::file_in(!!sprintf(
            "outputs/%s/%s_group_edmap_%s.tif", group, group, res[1])
          ),
          xlim = c(146.4, 148.13),
          ylim = c(-43.34, -42.33),
          legend_title = !!sprintf("log10(EL %skm)", round(res[1]/1000, 2)),
          set_value_range = c(!!globals$minimum_probability_for_maps, Inf),
          scale_type = "log10",
          transparency = 0.7,
          colramp_entire_range = TRUE,
          height = 7,
          nrow = 1,
          outfile = drake::file_out(
            !!sprintf("outputs/%s/static_maps/%s_group_edmap_hobart_%s.pdf", group, group, res[1])
          )
        ),

        group_adelaide_edmap = static_map(
          ras = drake::file_in(!!sprintf(
            "outputs/%s/%s_group_edmap_%s.tif", group, group, res[1])
          ),
          xlim = c(138, 139.5),
          ylim = c(-36, -34),
          legend_title = !!sprintf("log10(EL %skm)", round(res[1]/1000, 2)),
          set_value_range = c(!!globals$minimum_probability_for_maps, Inf),
          scale_type = "log10",
          transparency = 0.7,
          colramp_entire_range = TRUE,
          height = 7,
          nrow = 1,
          outfile = drake::file_out(
            !!sprintf("outputs/%s/static_maps/%s_group_edmap_adelaide_%s.pdf", group, group, res[1])
          )
        ),

        group_perth_edmap = static_map(
          ras = drake::file_in(!!sprintf(
            "outputs/%s/%s_group_edmap_%s.tif", group, group, res[1])
          ),
          xlim = c(115.28, 116.86),
          ylim = c(-32.56, -31.41),
          legend_title = !!sprintf("log10(EL %skm)", round(res[1]/1000, 2)),
          set_value_range = c(!!globals$minimum_probability_for_maps, Inf),
          scale_type = "log10",
          transparency = 0.7,
          colramp_entire_range = TRUE,
          height = 7,
          nrow = 1,
          outfile = drake::file_out(
            !!sprintf("outputs/%s/static_maps/%s_group_edmap_perth_%s.pdf", group, group, res[1])
          )
        ),

        group_darwin_edmap = static_map(
          ras = drake::file_in(!!sprintf(
            "outputs/%s/%s_group_edmap_%s.tif", group, group, res[1])
          ),
          xlim = c(130.5, 131.65),
          ylim = c(-12.88, -12.09),
          legend_title = !!sprintf("log10(EL %skm)", round(res[1]/1000, 2)),
          set_value_range = c(!!globals$minimum_probability_for_maps, Inf),
          scale_type = "log10",
          transparency = 0.7,
          colramp_entire_range = TRUE,
          height = 7,
          nrow = 1,
          outfile = drake::file_out(
            !!sprintf("outputs/%s/static_maps/%s_group_edmap_darwin_%s.pdf", group, group, res[1])
          )
        )
      )
      group_plan$target <- gsub(' ', '_', paste(group, group_plan$target))
      group_plan
    }) %>% dplyr::bind_rows()
  }

  return(unique(dplyr::bind_rows(plans)))
}
