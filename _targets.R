library(targets)
library(dplyr)
library(future)
library(future.apply)
library(parallelly)
library(edmaps)

n_cores <- max(1, floor(parallelly::availableCores(constraints='multicore') * 0.75))
tar_option_set(packages = c('terra', 'edmaps', 'future', 'future.apply', 'dplyr'))
message('no. of cores: ', n_cores)
future::multicore(workers = n_cores)
source('R/get_gbif_records.R')

# https://wlandau.github.io/targetopia/contributing.html#target-factories
get_excel_globals <- function(file) {

  globals <- readxl::read_excel(file, sheet='Global parameters', skip=1,
                                col_types=c('text', 'skip', 'list', 'skip')) %>%
    tidyr::pivot_wider(names_from=Variable, values_from=Value) %>%
    dplyr::select(output_crs=`Coordinate reference system (EPSG code)`,
                  output_res=`Output resolution`,
                  template_raster=`Template raster path`,
                  make_interactive_maps=`Make interactive maps?`,
                  gbif_username=`GBIF username`,
                  gbif_password=`GBIF password`,
                  basemap_mode=`Basemap mode`,
                  minimum_probability_for_maps=`Minimum probability threshold`,
                  landuse_path=`Land use class raster path`,
                  vegetation_path=`Vegetation class raster path`,
                  ndvi_path=`NDVI raster path`,
                  processed_data_path=`Processed data directory`) %>%
    as.list %>%
    lapply(unlist)

  paths_globals <- globals %>%
    .[grep('_path$', names(.))] %>%
    unlist() %>%
    setdiff(NA)

  if(length(paths_globals) > 0 && !all(file.exists(paths_globals))) {
    stop('File not found:\n    - ',
         paste0(paths_globals[!file.exists(paths_globals)], collapse='\n    - '))
  }

  if(is.na(globals$template_raster)) {
    # landuse <- terra::rast(globals$landuse_path)
    # if(is.na(globals$output_crs)) {
    #   stop('No coordinate reference system or template raster provided ',
    #        'on "Global parameters" sheet.')
    # }
    # if(is.na(globals$output_res)) {
    #   stop('No output resolution or template raster provided ',
    #        'on "Global parameters" sheet.')
    # }
    # warning('No template raster provided; outputs will use extent of land use raster.')
    stop('No template raster provided. See "Global parameters" sheet.')
  }
  # } else {
  #   if(is.na(globals$output_crs)) {
  #     globals$output_crs <- terra::crs(landuse)
  #   }
  #   if(is.na(globals$output_res)) {
  #     globals$output_crs <- terra::xres(landuse)
  #   }
  #   template_command <- substitute(terra::wrap(terra::rast()))
  # }

  # not currently implementing res and crs inputs from excel globals
  globals$output_crs <- NULL
  globals$output_res <- NULL

  template <- terra::rast(globals$template_raster)
  if(terra::crs(template) == '') {
    stop('template raster must have a CRS defined.') # alternatively we could assume it's WGS84?
  }

  globals

  # targets_global <- mapply(function(name, x) {
  #   if(grepl('_path$', name)) {
  #     tar_target_raw(name, x, format='file')
  #   } else {
  #     tar_target_raw(name, x)
  #   }
  # }, names(globals), globals)
  #
  # #tar_target_raw('globals', command)
  #
  # c(targets_global, list(
  #   tar_target(output_crs, terra::crs(terra::rast(template_raster))),
  #   tar_target(output_res, terra::xres(terra::rast(template_raster)))
  # ))
}

get_excel_species <- function(file) {
  species <- readxl::read_excel(file, sheet='Species') %>%
    dplyr::rename(species=`Species name`,
                  species_group=`Species group`,
                  landuse_classes=`Land use classes`,
                  vegetation_classes=`Vegetation classes`,
                  use_ndvi=`Use NDVI?`,
                  host_path=`Host raster`,
                  use_climate_suitability=`Use climate suitability`,
                  climate_suitability_path=`Climate suitability raster`,
                  gbif_species=`GBIF focal species name(s)`,
                  gbif_min_year=`GBIF min year`,
                  occurrence_path=`Occurrence csv file`,
                  infected_countries=`Infected countries`,
                  exclude_bioclim_vars =`Exclude BIOCLIM vars`
    ) %>%
    dplyr::mutate(
      species=gsub('^\\s+|\\s+$', '', species),
      species=gsub('\\s+', ' ', species),
      landuse_classes=edmaps:::expand_range(strsplit(
        gsub('^\\s+|\\s+$', '', landuse_classes), '\\s*,\\s*')),
      vegetation_classes=edmaps:::expand_range(strsplit(
        gsub('^\\s+|\\s+$', '', vegetation_classes), '\\s*,\\s*')),
      exclude_bioclim_vars=strsplit(
        gsub('^\\s+|\\s+$', '', exclude_bioclim_vars), '\\s*,\\s*'),
      infected_countries=gsub('^\\s+|\\s+$', '', infected_countries),
      use_ndvi=use_ndvi=='yes',
      use_climate_suitability=use_climate_suitability=='yes'
    ) %>%
    dplyr::filter(!is.na(species))

  invalid_abiotic <- species$use_climate_suitability &
    (is.na(species$occurrence_path) &
       is.na(species$gbif_species) &
       is.na(species$climate_suitability_path))

  if(any(invalid_abiotic)) {
    # ^ if abiotic weight is used, then if no occurrence/gbif data are used and
    #   climate suitability path not provided, stop.
    stop('If "Use climate suitability" is "yes", then either "Climate ',
         'suitability raster" or else "GBIF focal species name(s)" and/or ',
         '"Occurrence csv file" must be provided.')
  }

  cabi_paths_idx <- which(grepl('\\.csv$', species$infected_countries) &
                            species$use_climate_suitability)
  exist <- file.exists(species$infected_countries[cabi_paths_idx])

  if(any(!exist)) {
    stop('File not found:\n    - ',
         paste0(species$infected_countries[cabi_paths_idx[!exist]], collapse='\n    - '))
  }
  species$cabi_path <- NA
  species$cabi_path[cabi_paths_idx] <- species$infected_countries[cabi_paths_idx]
  species$infected_countries[cabi_paths_idx] <- NA

  paths_species <- species %>%
    dplyr::select(grep('_path$', colnames(.))) %>%
    unlist() %>%
    setdiff(NA)

  if(length(paths_species) > 0 && !all(file.exists(paths_species))) {
    stop('File not found:\n    - ',
         paste0(paths_species[!file.exists(paths_species)], collapse='\n    - '))
  }

  # Test if any countries are invalid
  if(any(!is.na(species$infected_countries))) {
    countries <- unique(unlist(strsplit(setdiff(species$infected_countries, NA),
                                        '\\s*,\\s*')))
    testmatch <- countrycode::countrycode(
      countries, 'country.name', 'iso3n', warn=FALSE)

    if(any(is.na(testmatch))) {
      stop('Unrecognised countries:\n    - ',
           paste0(countries[is.na(testmatch)], collapse='\n    - '),
           '\nSee countrycode::codelist$country.name.en for allowed values.')
    }
  }

  species <- species %>%
    dplyr::mutate(
      infected_countries=strsplit(
        gsub('^\\s+|\\s+$', '', infected_countries), '\\s*,\\s*'),
      gbif_species=strsplit(
        gsub('^\\s+|\\s+$', '', gbif_species), '\\s*,\\s*')
    )

  species
}


get_excel_pathways <- function(file) {
  pathways <- readxl::read_excel(file, sheet='Pathways') %>%
    dplyr::rename(
      species=`Species`, pathway=`Pathway name`,
      leakage_lower=`Leakage rate lower`, leakage_upper=`Leakage rate upper`,
      viability_lower=`Viability lower`, viability_upper=`Viability upper`,
      weights=`Arrival weights (raster)`
    ) %>%
    dplyr::mutate(
      species=gsub('^\\s+|\\s+$', '', species),
      species=gsub('\\s+', ' ', species)
    )

  pathways
}

make_plan <- function(file) {
  globals <- get_excel_globals(file)
  list(
    tar_target_raw('file', substitute(file), format='file'),
    tar_target(globals, get_excel_globals(file)),
    # separate globals into individual targets to prevent outdating of all
    # params when a single param changes.
    lapply(names(globals), function(x) tar_target_raw(x, parse(text=sprintf('globals$%s', x)))),
    tar_target(output_res, terra::xres(terra::rast(template_raster))),
    tar_target(output_extent, as.vector(terra::ext(terra::rast(template_raster)))),
    tar_target(species, get_excel_species(file)),
    tar_target(pathways, get_excel_pathways(file)),
    tar_target(
      landuse,
      edmaps::rle_compress(x=terra::rast(landuse_path), quiet=TRUE),
      format='qs'
    ),
    tar_target(landuse_classes, {
      species %>%
        dplyr::filter(lengths(landuse_classes) > 0) %>%
        dplyr::mutate(classes=lapply(landuse_classes, sort)) %>%
        dplyr::group_by(classes) %>%
        dplyr::summarise(species=list(species))
    }),
    tar_target(vegetation_classes, {
      species %>%
        dplyr::filter(lengths(vegetation_classes) > 0) %>%
        dplyr::mutate(classes=lapply(vegetation_classes, sort)) %>%
        dplyr::group_by(classes) %>%
        dplyr::summarise(species=list(species))
    }),
    tar_target(landuse_ok, {
      terra::wrap(binarize_and_aggregate(
        rle=landuse, categories=landuse_classes$classes[[1]],
        res=output_res, extent=output_extent, return_rast=TRUE, quiet=TRUE
      ))
    }, pattern=map(landuse_classes), iteration='list'),
    tar_target(
      vegetation,
      edmaps::rle_compress(x=terra::rast(vegetation_path), quiet=TRUE),
      format='qs'
    ), # todo: dynamic target: only if ≥0 species use vegetation classes
    tar_target(vegetation_ok, {
      terra::wrap(binarize_and_aggregate(
        rle=vegetation, categories=vegetation_classes$classes[[1]],
        res=output_res, extent=output_extent, quiet=TRUE
      ))
    }, pattern=map(vegetation_classes), iteration='list'),
    # accommodate lacking veg classes
    tar_target(ndvi, {
      if(any(species$use_ndvi)) {
        terra::wrap(
          terra::project(terra::rast(ndvi_path), terra::rast(template_raster),
                         method='bilinear')
        )
      } else {
        NULL
      }
    }), # ^ dynamic target: only if ≥0 species use ndvi
    # tar_target(host_rast, {
    #
    # })
    tar_target(biotic_suitability, {
      result <- list()
      if(!is.na(species$host_path)) {
        result <- append(result, list(terra::rast(species$host_path)))
      }

      if(length(setdiff(species$landuse_classes[[1]], NA)) > 0) {
        result <- append(
          result,
          list(terra::rast(
            landuse_ok[[grep(species$species, landuse_classes$species, fixed=TRUE)]]
          ))
        )
      }

      if(length(setdiff(species$vegetation_classes[[1]], NA)) > 0) {
        result <- append(
          result,
          list(terra::rast(
            vegetation_ok[[grep(species$species, vegetation_classes$species, fixed=TRUE)]]
          ))
        )
      }

      if(species$use_ndvi) {
        result <- append(result, list(terra::rast(ndvi)))
      }

      if(length(result)==0) {
        # If no user host rast, no veg classes, no landuse classes, and no ndvi,
        # then return NULL, else calculate the prod of the list elements and
        # return that, wrapped.
        NULL
      } else {
        terra::wrap(Reduce(`*`, result))
      }
    }, pattern=map(species), iteration='list'),
    tar_target(occurrences, {
      if(!species$use_climate_suitability || !is.na(species$climate_suitability_path)) {
        return(NULL)
      } else {
        occ <- list()
        if(!is.na(species$occurrence_path)) {
          occ$user <- read.csv(species$occurrence_path)[, c('Longitude', 'Latitude')]
        }
        if(!all(is.na(unlist(species$gbif_species)))) {
          args <- list(
            taxon=unlist(species$gbif_species), coord_uncertainty=20000,
            email='cebra.apps@gmail.com'
          )
          args$min_year <- if(!is.na(species$gbif_min_year)) {
            species$gbif_min_year
          } else {
            1970
          }
          if(!is.na(gbif_username) & !is.na(gbif_password)) {
            args$username <- gbif_username
            args$pwd <- gbif_password
            args$method <- 'download'
          }
          occ$gbif <- dplyr::select(
            do.call(get_gbif_records, args),
            Longitude=decimalLongitude, Latitude=decimalLatitude,
            dplyr::everything()
          )
        }
        dplyr::bind_rows(occ, .id='source')
      }
    }, pattern=map(species), iteration='list'),
    tar_target(abiotic_suitability, {
      if(!species$use_climate_suitability) {
        return(1)
      } else {
        # if a climate raster is provided, use it
        if(!is.na(species$climate_suitability_path)) {
          r <- terra::rast(species$climate_suitability_path)
          terra::setMinMax(r)
          rng <- terra::minmax(r)
          if(rng[1] < 0 || rng[2] > 1) {
            stop('Invalid climate suitability values for species ',
                 species$species, ' Values must be within the range 0-1.')
          }
        } else if(!is.null(occurrences)) {
          head(occurrences) # todo: testing
        }
      }
    }, pattern=map(species, occurrences), iteration='list')
  )
}

make_plan('~/projects/cebra/20121001-multi-pest-early-detection-maps/data/user_input/parameters.xlsx')
