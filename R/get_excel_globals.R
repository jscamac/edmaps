#' Get global parameters from Excel file
#'
#' This function reads the "Global parameters" sheet from the specified Excel file and returns a list of global parameters.
#'
#' @param file The path to the Excel file.
#' @return A list of global parameters.
#' @examples
#' get_excel_globals(
#'   system.file("extdata", "parameters.xlsx", package = "edmaps")
#' )
#' @importFrom dplyr select
#' @importFrom readxl read_excel
#' @importFrom sf st_as_sf st_segmentize
#' @importFrom terra as.polygons crs ext project rast res vect wrap
#' @importFrom tidyr pivot_wider
#' @export
get_excel_globals <- function(file) {
  globals <- readxl::read_excel(file,
    sheet = "Global parameters", skip = 1,
    col_types = c("text", "skip", "list", "skip")
  ) %>%
    tidyr::pivot_wider(names_from = Variable, values_from = Value) %>%
    dplyr::select(
      output_crs = `Coordinate reference system (EPSG code)`,
      output_res = `Output resolution`,
      worldclim_res = `WorldClim resolution`,
      template_raster = `Template raster path`,
      gbif_username = `GBIF username`,
      gbif_email = `GBIF email address`,
      make_interactive_maps = `Make interactive maps?`,
      basemap_mode = `Basemap mode`,
      minimum_probability_for_maps = `Minimum probability threshold`,
      landuse_path = `Land use class raster path`,
      vegetation_path = `Vegetation class raster path`,
      ndvi_path = `NDVI raster path`,
      processed_data_path = `Processed data directory`
    ) %>%
    lapply(unlist)

  paths_globals <- globals %>%
    .[grep("_path$", names(.))] %>%
    unlist() %>%
    setdiff(NA)

  if (length(paths_globals) > 0) {
    missing_files <- paths_globals[!file.exists(paths_globals)]
    if (length(missing_files) > 0) {
      stop(sprintf(
        "File specified in Global parameters tab of %s not found:\n    - ",
        file
      ), paste0(missing_files, collapse = "\n    - "))
    }
  }

  if (is.na(globals$template_raster)) {
    stop('No template raster provided. See "Global parameters" sheet.')
  }
  template <- terra::rast(globals$template_raster)

  if (terra::crs(template) == "") {
    terra::crs(template) <- "EPSG:4326"
    warning("template raster has undefined CRS - WGS84 (EPSG:4326) assumed.")
  }

  e <- terra::ext(template)
  if (is.na(globals$output_crs)) {
    globals$output_crs <- terra::crs(template)
  } else {
    e_poly <- terra::as.polygons(e, crs = terra::crs(template))
    e_poly2 <- terra::vect(sf::st_segmentize(sf::st_as_sf(e_poly), dfMaxLength = 10000))
    e <- terra::ext(terra::project(e_poly2, globals$output_crs))
    # TODO: ^ double-check this - need to segmentize the extent before
    # projecting (to ensure we capture the encompassing extent), but should tidy
    # this up.
    # TODO: look at `align` arg of project as an alternative?
  }
  if (is.na(globals$output_res)) {
    globals$output_res <- unique(terra::res(template))
    if (length(globals$output_res) > 1) {
      stop("Invalid template raster. Output must have equal vertical and horizontal resolution.")
    }
  }
  # TODO: capture case where global output res (etc) is an in valid input (e.g.
  # string). This could potentially silently be coerced to NA, which is
  # dangerous.

  globals$output_extent <- check_extent(as.vector(e),
    res = globals$output_res,
    snap = "out"
  )

  # template_command <- substitute(terra::wrap(terra::rast()))
  globals
}
