#' Make Plan
#'
#' This function generates a plan for the target pipeline.
#'
#' @param file The path to the Excel file.
#' @return A list of targets for the pipeline.
#' @importFrom config get
#' @importFrom dplyr bind_rows everything filter group_by mutate select summarise
#' @importFrom terra clamp crs ext minmax project rast setMinMax unwrap wrap xres
#' @examples
#' make_edmaps(
#'   system.file("extdata", "parameters.xlsx", package = "edmaps")
#' )
#' @export
make_edmaps <- function(file) {
  globals <- get_excel_globals(file)
  list(
    tar_target_raw("file", substitute(file), format = "file"),
    tar_target(globals, get_excel_globals(file)),
    # separate globals into individual targets to prevent outdating of all
    # params when a single param changes.
    lapply(names(globals), function(x) tar_target_raw(x, parse(text = sprintf("globals$%s", x)))),
    # tar_target(output_res, terra::xres(terra::rast(template_raster))),
    # tar_target(output_extent, as.vector(terra::ext(terra::rast(template_raster)))),
    tar_target(species, get_excel_species(file)),
    tar_target(pathways, get_excel_pathways(file)),
    tar_target(
      landuse,
      rle_compress(x = terra::rast(landuse_path), quiet = TRUE),
      format = "qs"
    ),
    tar_target(landuse_classes, {
      species %>%
        dplyr::filter(lengths(landuse_classes) > 0) %>%
        dplyr::mutate(classes = lapply(landuse_classes, sort)) %>%
        dplyr::group_by(classes) %>%
        dplyr::summarise(species = list(species))
    }),
    tar_target(vegetation_classes, {
      species %>%
        dplyr::filter(lengths(vegetation_classes) > 0) %>%
        dplyr::mutate(classes = lapply(vegetation_classes, sort)) %>%
        dplyr::group_by(classes) %>%
        dplyr::summarise(species = list(species))
    }),
    tar_target(host_files, {
      species %>%
        dplyr::filter(!is.na(host_path)) %>%
        dplyr::group_by(host_path) %>%
        dplyr::summarise(species = list(species))
    }),
    tar_target(climsuit_files, {
      species %>%
        dplyr::filter(!is.na(climate_suitability_path)) %>%
        dplyr::group_by(climate_suitability_path) %>%
        dplyr::summarise(species = list(species))
    }),
    tar_target(landuse_ok,
      {
        terra::wrap(binarize_and_aggregate(
          rle = landuse, categories = landuse_classes$classes[[1]],
          extent = output_extent, crs = output_crs, res = output_res,
          return_rast = TRUE, quiet = TRUE
        ))
      },
      pattern = map(landuse_classes),
      iteration = "list"
    ),
    tar_target(
      vegetation,
      rle_compress(x = terra::rast(vegetation_path), quiet = TRUE),
      format = "qs"
    ), # TODO dynamic target: only if ≥0 species use vegetation classes

    tar_target(vegetation_ok,
      {
        terra::wrap(binarize_and_aggregate(
          rle = vegetation, categories = vegetation_classes$classes[[1]],
          extent = output_extent, crs = output_crs, res = output_res,
          return_rast = TRUE, quiet = TRUE
        ))
      },
      pattern = map(vegetation_classes),
      iteration = "list"
    ),
    # TODO accommodate lacking veg classes (e.g. with if(nrow(vegetation_classes==0)))

    tar_target(ndvi, {
      if (!any(species$use_ndvi)) {
        return(NULL)
      }
      r <- terra::rast(ndvi_path)
      if (terra::crs(r) == "") {
        terra::crs(r) <- "EPSG:4326"
        warning("ndvi raster has undefined CRS - WGS84 (EPSG:4326) assumed.")
      }
      if (terra::crs(r) != output_crs || terra::xres(r) != output_res ||
        !all(as.vector(terra::ext(r)) == output_extent)) {
        r <- terra::project(
          r,
          terra::rast(ext = output_extent, resolution = output_res, crs = output_crs),
          # TODO: ^could replace these expressions with an updated template raster
          method = "bilinear"
        )
        r <- terra::clamp(r, lower = 0, values = TRUE)
      }
      terra::wrap(r)
    }), # ^ dynamic target: only if ≥0 species use ndvi

    tar_target(host_rast,
      {
        if (nrow(host_files) == 0) {
          return(NULL)
        }
        r <- terra::rast(host_files$host_path) > 0
        # ^ all non-zero, non-NA cells are considered host
        if (terra::crs(r) == "") {
          terra::crs(r) <- "EPSG:4326"
          warning(sprintf(
            "%s has undefined CRS - WGS84 (EPSG:4326) assumed.",
            host_files$host_path
          ))
        }
        if (terra::crs(r) != output_crs || terra::xres(r) != output_res ||
          !all(as.vector(terra::ext(r)) == output_extent)) {
          r <- terra::project(
            r,
            terra::rast(ext = output_extent, resolution = output_res, crs = output_crs),
            method = "near"
          )
        }
        terra::wrap(r)
      },
      pattern = map(host_files),
      iteration = "list"
    ),
    tar_target(climsuit_rast,
      {
        if (nrow(climsuit_files) == 0) {
          return(NULL)
        }
        r <- terra::rast(climsuit_files$climate_suitability_path)
        if (terra::crs(r) == "") {
          terra::crs(r) <- "EPSG:4326"
          warning(sprintf(
            "%s has undefined CRS - WGS84 (EPSG:4326) assumed.",
            host_files$host_path
          ))
        }
        if (terra::crs(r) != output_crs || terra::xres(r) != output_res ||
          !all(as.vector(terra::ext(r)) == output_extent)) {
          r <- terra::project(
            r,
            terra::rast(ext = output_extent, resolution = output_res, crs = output_crs),
            method = "bilinear"
          )
        }
        terra::setMinMax(r)
        rng <- terra::minmax(r)
        if (rng[1] < 0 || rng[2] > 1) {
          stop(
            "Invalid climate suitability values for file ",
            climsuit_files, ". Values must be within the range 0-1."
          )
        }
        terra::wrap(r)
      },
      pattern = map(climsuit_files),
      iteration = "list"
    ),
    tar_target(biotic_suitability,
      {
        result <- list()

        use_host_path <- !is.na(species$host_path[[1]])
        use_landuse <- length(setdiff(species$landuse_classes[[1]], NA)) > 0
        use_vegetation <- length(setdiff(species$vegetation_classes[[1]], NA)) > 0

        if (use_host_path) {
          result$user <- terra::rast(
            host_rast[[grep(species$species, host_files$species, fixed = TRUE)]]
          )
        }

        if (use_landuse) {
          result$landuse <- terra::rast(
            landuse_ok[[grep(species$species, landuse_classes$species, fixed = TRUE)]]
          )
        }

        if (use_vegetation) {
          result$vegetation <- terra::rast(
            vegetation_ok[[grep(species$species, vegetation_classes$species, fixed = TRUE)]]
          )
        }

        if (!(use_host_path || use_landuse || use_vegetation)) {
          result <- 1L # TODO handle case where abiotic_suitability = 1 and biotic_suitability = 1 (need to return a raster)
        } else {
          result <- terra::wrap(Reduce(`|`, result))
        }

        if (species$use_ndvi) {
          terra::wrap(terra::unwrap(result) * terra::rast(ndvi))
        } else {
          result
        }
      },
      pattern = map(species),
      iteration = "list"
    ),
    tar_target(occurrences,
      {
        if (!species$use_climate_suitability || !is.na(species$climate_suitability_path)) {
          return(NULL)
        }
        occ <- list()
        if (!is.na(species$occurrence_path)) {
          user <- read.csv(species$occurrence_path)
          if (!all(c("Longitude", "Latitude") %in% names(user))) {
            stop(
              "User occurrence csv (", species$occurrence_path,
              ') must specify coordinates in columns named "Longitude" and "Latitude".'
            )
          }
          occ$user <- user[, c("Longitude", "Latitude")]
        }
        if (any(!is.na(unlist(species$gbif_species)))) {
          args <- list(
            taxon = unlist(species$gbif_species), coord_uncertainty = 20000
          )
          args$min_year <- if (!is.na(species$gbif_min_year)) {
            species$gbif_min_year
          } else {
            1970
          }
          if (file.exists("config.yml")) {
            user <- config::get("gbif_username")
            pw <- config::get("gbif_password")
            email <- config::get("gbif_email")
            if (!is.null(user) && !is.null(pw) && !is.null(email)) {
              args$username <- user
              args$pw <- pw
              args$email <- email
            }
          }
          occ$gbif <- dplyr::select(
            do.call(get_gbif_records, args),
            Longitude = decimalLongitude, Latitude = decimalLatitude,
            dplyr::everything()
          )
          # TODO: warn if no records returned from GBIF
          # TODO: could reduce this to Longitude, Latitude cols
        }
        dplyr::bind_rows(occ, .id = "source")
      },
      pattern = map(species),
      iteration = "list",
      resources = tar_resources(
        crew = tar_resources_crew(controller = "controller_single") # TODO figure out how best to pass this as an argument to make_edmaps
      )
    ),
    # ^ prevent rate limits

    # if(!is.na(species$host_path[[1]])) {
    #   result <- append(
    #     result,
    #     list(terra::rast(
    #       host_rast[[grep(species$species, host_files$species, fixed=TRUE)]]
    #     ))
    #   )
    # }

    tar_target(worldclim,
      {
        if (all(sapply(occurrences, is.null))) {
          return(NULL)
        }
        f <- tempfile()
        res <- switch(worldclim_res,
          `30 seconds` = "30s",
          `2.5 minutes` = "2.5m",
          `5 minutes` = "5m",
          `10 minutes` = "10m"
        )
        d <- file.path("data/worldclim", res)
        download_worldclim2(f, "bio", res)
        extract_worldclim2(f, d)
      },
      format = "file"
    ),
    tar_target(abiotic_suitability,
      {
        if (!species$use_climate_suitability) {
          if (!is.na(species$gbif_species)) {
            warning(
              "GBIF species names ignored for ", species$species,
              "(Use climate suitability = FALSE)."
            )
          }
          if (!is.na(species$climate_suitability_path)) {
            warning(
              "Climate suitability raster ignored for ", species$species,
              "(Use climate suitability = FALSE)."
            )
          }
          return(1)
        } else {
          # Otherwise, if a climate raster is provided, use it
          if (!is.na(species$climate_suitability_path)) {
            if (!is.na(species$gbif_species)) {
              warning(
                "GBIF species names ignored for ", species$species,
                "(Climate suitability raster provided)."
              )
            }
            r <- terra::unwrap(
              climsuit_rast[[grep(species$species, climsuit_files$species, fixed = TRUE)]]
            )
            terra::wrap(r) # TODO rethink this... duplicates this rast in cache
          } else if (!is.null(occurrences)) {
            # If no climate raster is provided, but GBIF occurrences were retrieved...
            clim <- terra::rast(worldclim)
            exclude <- if (is.na(species$exclude_bioclim_vars[[1]])) {
              NULL
            } else {
              species$exclude_bioclim_vars[[1]]
            }
            r <- range_bag(
              occurrences, clim,
              n_dims = 2, n_models = 100, p = 0.5,
              exclude_vars = exclude
            )
            if (terra::crs(r) != output_crs || terra::xres(r) != output_res ||
              !all(as.vector(terra::ext(r)) == output_extent)) {
              r <- terra::project(
                r,
                terra::rast(ext = output_extent, resolution = output_res, crs = output_crs),
                method = "bilinear"
              )
            }
            terra::wrap(r)
          } else {
            # TODO: need to handle case where use_climate_suitability = TRUE but no GBIF occurrences are available. Here we just `stop` but we need to handle this more gracefully.
            # If no climate raster is provided, and no GBIF occurrences were retrieved...
            stop(
              "No climate suitability raster provided and no GBIF occurrences ",
              "available for ", species$species,
              ". Ensure that GBIF species names are specified ",
              "correctly, or set 'Use climate suitability' to 'no' to disable ",
              "climate suitability."
            )
          }
        }
      },
      pattern = map(species, occurrences),
      iteration = "list"
    ),
    tar_target(suitability,
      {
        r <- if (is.atomic(abiotic_suitability) && is.atomic(biotic_suitability)) {
          warning(
            "Cannot calculate suitability for ", species$species,
            ". Returning raster filled with zeroes. If this is unexpected, ",
            "check Excel input file and ensure that fields permitting ",
            "calculation of climate suitability or biotic suitability (or both)",
            "are correctly populated."
          )
          terra::rast(
            ext = output_extent, resolution = output_res, crs = output_crs,
            vals = 0
          )
        } else {
          terra::unwrap(abiotic_suitability) * terra::unwrap(biotic_suitability) # TODO: do either of the components need to be scaled/normalised first?
        }
        terra::wrap(r)
      },
      pattern = map(species, abiotic_suitability, biotic_suitability),
      iteration = "list"
    ),

    tar_target(pathway_weight_paths, {
      pathways %>%
        dplyr::mutate(weights = normalizePath(weights)) %>% # move this to pathways target (req'd to make arrivals target robust)
        dplyr::group_by(weights) %>%
        dplyr::summarise(species = list(sort(unique(species))))
    }),
    tar_target(
      weight_rasters,
      {
        r <- terra::rast(pathway_weight_paths$weights)
        if (terra::crs(r) != output_crs || terra::xres(r) != output_res ||
          !all(as.vector(terra::ext(r)) == output_extent)) {
          r <- terra::project(
            r,
            terra::rast(
              ext = output_extent, resolution = output_res, crs = output_crs
            ),
            method = "bilinear"
          )
        }
        terra::setMinMax(r)
        if (terra::minmax(r)[1] < 0) {
          r[r < 0] <- 0
          warning(
            "Negative values in pathway weight raster ",
            pathway_weight_paths$weights, " were truncated to 0."
          )
        }
        r <- r / terra::minmax(r)[2] # TODO document that we are normalising by dividing by max and truncating at 0
        terra::wrap(r)
      },
      pattern = map(pathway_weight_paths),
      iteration = "list"
    ),
    tar_target(
      test, {
        i <- grep(species$species, pathway_weight_paths$species, fixed = TRUE)
        if(length(i) == 0) return(NULL)
        weight_rasters[i] %>%
          lapply(terra::unwrap) %>%
          terra::rast() %>%
          terra::wrap()
      },
      pattern = map(species),
      iteration = 'list'
    ),
    tar_target(
      arrivals_by_pathway, {
        # subset pathways to the rows where species is in species sheet
        # TODO replace spaces with underscores in pathway names?
        sp <- species$species
        dat <- pathways %>%
          dplyr::filter(species == sp)
        if(nrow(dat) == 0) {
          warning("No pathways found for species ", sp)
          return(NULL)
        }
        mapply(
          function(p, ll, lu, vl, vu, w) {
            w <- normalizePath(w) # TODO remove normalizePath() once we move this to pathways target
            message(w)
            edmaps::arrivals(
              weight = terra::unwrap(weight_rasters[[which(pathway_weight_paths$weights == w)]]), 
              leakage = c(ll, lu), viability = c(vl, vu), 
              return_rast = TRUE
            )
          }, 
          dat$pathway, dat$leakage_lower, dat$leakage_upper,
          dat$viability_lower, dat$viability_upper, dat$weights,
          SIMPLIFY = FALSE
        ) %>% terra::rast() %>%
        terra::wrap()
      },
      pattern = map(species),
      iteration = 'list'
    ),
    tar_target(
      arrivals, {
        if(is.null(arrivals_by_pathway))  return(NULL)
        r <- 1 - prod(1 - terra::unwrap(arrivals_by_pathway))
        terra::wrap(r)
      },
      pattern = map(arrivals_by_pathway),
      iteration = 'list'
    ),
    tar_target(
      edmap, {
        if(is.null(arrivals)) return(NULL)
        establishment_likelihood(
          total_arrivals = terra::unwrap(arrivals),
          suitability = terra::unwrap(suitability),
          return_rast = TRUE
        ) %>% terra::wrap()
      },
      pattern = map(species, arrivals, suitability),
      iteration = 'list'
    ),
    tar_target(
      plot_edmap, {
       
      },
      pattern = map(edmap),
      iteration = 'list'
    ),
  )
}
