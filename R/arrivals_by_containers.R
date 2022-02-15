#' Estimates pest arrivals by containers
#'
#' Estimates pest arrivals by containers.
#'
#' @param container_weights An [`sf`] object or file path to a
#'   shapefile supported by OGR, as produced by [container_weights()].
#' @param port_data Character. Path to csv file containing Port Names, Port
#'   Codes, Longitude, Latitude and Container volumes. Column names must be (in
#'   this order): Name, PortCode, Longitude, Latitude, Count.
#' @param template_raster `RasterLayer` or file path to a raster file.
#'   This is used to define the extent and resolution of output. Must be in
#'   CRS EPSG:3577.
#' @param leakage_rate Numeric vector of 2 values, giving the lower and upper
#'   bounds of a 95% CI for leakage rate (the number of pest leakage events in
#'   a random year).
#' @param establishment_rate Numeric vector of 2 values, giving the lower and
#'   upper bounds of a 95% CI for establishment rate (the rate of survival, to
#'   establishment, for leakage events).
#' @param outfile Character. Output raster file path. If `probability` has
#'   length > 1, the file type must support multiband rasters (e.g. GeoTiff). If
#'   not provided, raster object will be returned to R.
#' @param return_rast Logical. Should the raster object be returned to R?
#'   Ignored if `outfile` is not provided.
#' @param overwrite Logical. If `TRUE` and `outfile` is not missing,
#'   it will be overwritten if the file specified by `outfile` already
#'   exists.
#' @return If `outfile` is specified, the resulting raster (multiband if
#'   `probability` has length > 1) is saved as a tiff at that path. If
#'   `return_rast` is `TRUE` or `outfile` is not specified the
#'   resulting raster object is returned, otherwise `NULL` is returned
#'   invisibly.
#' @family functions estimating arrivals
#' @importFrom sf read_sf st_drop_geometry
#' @importFrom raster raster stack writeRaster
#' @importFrom utils read.csv
#' @importFrom dplyr filter mutate
#' @importFrom fasterize fasterize
#' @export
#'
arrivals_by_containers <- function(container_weights, port_data,
  template_raster, leakage_rate, establishment_rate, outfile,
  return_rast=FALSE, overwrite=FALSE) {

  # Load container weight data if path provided
  if(is.character(container_weights)) {
    container_weights <- sf::read_sf(container_weights)
  }

  container_weights$POA_CODE <- as.numeric(container_weights$POA_CODE)

  # Load port_data
  port_data <- utils::read.csv(port_data)
  # Load template_raster
  if(is.character(template_raster)) {
    template_raster <- raster::raster(template_raster)
  } else if(!is(template_raster, 'RasterLayer')) {
    stop('template_raster must be a RasterLayer or a file path to a raster file.')
  }

  # Subset port data to ports present in container weights
  port_data <- port_data %>%
    dplyr::filter(Name %in% names(container_weights)) %>%
    dplyr::mutate(Name = as.character(Name))

  # Multiply proportional postcode distribution volumes by port container
  # arrival volumes, and sum resulting postcode arrivals across source ports
  container_weights$total <-
    rowSums(sweep(sf::st_drop_geometry(container_weights[, port_data$Name]), 2,
        port_data$Count, `*`))

  # Convert total postcode arrivals to a proportion of their sum
  container_weights$total <-
    container_weights$total/sum(container_weights$total, na.rm=TRUE)

  # COMMENT THIS
  postcode_rast <- container_weights %>%
    fasterize::fasterize(raster = template_raster, field = "POA_CODE")

  postcode_n <- as.data.frame(postcode_rast) %>%
    na.omit %>%
    dplyr::group_by(layer) %>%
    dplyr::summarise(n_cells=dplyr::n())

  container_weights <- container_weights %>%
    dplyr::left_join(postcode_n, by=c(POA_CODE='layer')) %>%
    dplyr::mutate(total_per_cell=total/n_cells)

  # Rasterize postcode arrivals
  weight <- fasterize::fasterize(sf=container_weights, raster=template_raster,
                                 field='total_per_cell')

  # Rescale due to missing postcodes not being rasterised (not overlaying cell
  # centres).
  weight <- calc_proportion(weight)

  # Disperse passengers and calculate arrival rate
  EE <- calc_EE(leakage_rate, establishment_rate)
  out <- calc_pathway_pr(EE, weight, return_rast=TRUE)

  if(!missing(outfile)) {
    # Create directory if it does not exist
    if(!dir.exists(dirname(outfile))) {
      dir.create(dirname(outfile), recursive = TRUE)
    }

    # write out raster
    raster::writeRaster(out, outfile, overwrite = overwrite)
  }
  if(isTRUE(return_rast) || missing(outfile)) {
    out
  } else {
    invisible(NULL)
  }
}
