#' Get species data from an Excel file
#'
#' This function reads in an Excel file and extracts the species data from the
#' "Species" sheet. It then renames the columns to match the expected input
#' format, and performs some data cleaning. Finally, it returns the cleaned
#' species data.
#'
#' @param file The path to the Excel file.
#' @return A data frame containing the cleaned species data.
#' @examples
#' get_excel_species(
#'   system.file("extdata", "parameters.xlsx", package = "edmaps")
#' )
#' @importFrom readxl read_excel
#' @importFrom dplyr rename mutate filter select
#' @importFrom countrycode countrycode
#' @export
get_excel_species <- function(file) {
  species <- readxl::read_excel(file, sheet = "Species") %>%
    dplyr::rename(
      species = `Species name`,
      species_group = `Species group`,
      landuse_classes = `Land use classes`,
      vegetation_classes = `Vegetation classes`,
      use_ndvi = `Use NDVI?`,
      host_path = `Host raster`,
      use_climate_suitability = `Use climate suitability`,
      climate_suitability_path = `Climate suitability raster`,
      gbif_species = `GBIF focal species name(s)`,
      gbif_min_year = `GBIF min year`,
      occurrence_path = `Occurrence csv file`,
      infected_countries = `Infected countries`,
      exclude_bioclim_vars = `Exclude BIOCLIM vars`
    ) %>%
    dplyr::mutate(
      species = gsub("^\\s+|\\s+$", "", species),
      species = gsub("\\s+", " ", species),
      landuse_classes = edmaps:::expand_range(strsplit(
        gsub("^\\s+|\\s+$", "", landuse_classes), "\\s*,\\s*"
      )),
      vegetation_classes = edmaps:::expand_range(strsplit(
        gsub("^\\s+|\\s+$", "", vegetation_classes), "\\s*,\\s*"
      )),
      exclude_bioclim_vars = strsplit(
        gsub("^\\s+|\\s+$", "", exclude_bioclim_vars), "\\s*,\\s*"
      ),
      infected_countries = gsub("^\\s+|\\s+$", "", infected_countries),
      use_ndvi = use_ndvi == "yes",
      use_climate_suitability = use_climate_suitability == "yes"
    ) %>%
    dplyr::filter(!is.na(species))

  invalid_abiotic <- species$use_climate_suitability &
    (is.na(species$occurrence_path) &
      is.na(species$gbif_species) &
      is.na(species$climate_suitability_path))

  if (any(invalid_abiotic)) {
    # ^ if abiotic weight is used, then if no occurrence/gbif data are used and
    #   climate suitability path not provided, stop.
    stop(
      'If "Use climate suitability" is "yes", then either "Climate ',
      'suitability raster" or else "GBIF focal species name(s)" and/or ',
      '"Occurrence csv file" must be provided.'
    )
  }

  cabi_paths_idx <- which(grepl("\\.csv$", species$infected_countries) &
    species$use_climate_suitability)
  exist <- file.exists(species$infected_countries[cabi_paths_idx])

  if (any(!exist)) {
    stop(
      "File not found:\n    - ",
      paste0(
        species$infected_countries[cabi_paths_idx[!exist]],
        collapse = "\n    - "
      )
    )
  }
  species$cabi_path <- NA
  species$cabi_path[cabi_paths_idx] <- species$infected_countries[cabi_paths_idx]
  species$infected_countries[cabi_paths_idx] <- NA

  paths_species <- species %>%
    dplyr::select(grep("_path$", colnames(.))) %>%
    unlist() %>%
    setdiff(NA)

  if (length(paths_species) > 0 && !all(file.exists(paths_species))) {
    stop(
      "File not found:\n    - ",
      paste0(paths_species[!file.exists(paths_species)], collapse = "\n    - ")
    )
  }

  # Test if any countries are invalid
  if (any(!is.na(species$infected_countries))) {
    countries <- unique(unlist(strsplit(
      setdiff(species$infected_countries, NA),
      "\\s*,\\s*"
    )))
    testmatch <- countrycode::countrycode(
      countries, "country.name", "iso3n",
      warn = FALSE
    )

    if (any(is.na(testmatch))) {
      stop(
        "Unrecognised countries:\n    - ",
        paste0(countries[is.na(testmatch)], collapse = "\n    - "),
        "\nSee countrycode::codelist$country.name.en for allowed values."
      )
    }
  }

  species <- species %>%
    dplyr::mutate(
      infected_countries = strsplit(
        gsub("^\\s+|\\s+$", "", infected_countries), "\\s*,\\s*"
      ),
      gbif_species = strsplit(
        gsub("^\\s+|\\s+$", "", gbif_species), "\\s*,\\s*"
      )
    )

  species
}
