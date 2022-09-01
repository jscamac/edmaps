#' Estimates pest arrivals by containers
#'
#' Estimates pest arrivals by containers.
#'
#' @param container_weights A [`SpatVector`] or [`sf`] object, or a file path to
#'   a vector dataset in a format supported by OGR, as produced by
#'   [container_weights()].
#' @param port_data Character. Path to csv file containing Port Names, Port
#'   Codes, Longitude, Latitude and Container volumes. Column names must be (in
#'   this order): Name, PortCode, Longitude, Latitude, Count.
#' @param template_raster [`SpatRaster`] or `RasterLayer` object, or a file
#'   path to a raster file. This is used to define the extent and resolution of
#'   output. Must be specified in CRS EPSG:3577.
#' @param leakage_rate Numeric vector of 2 values, giving the lower and upper
#'   bounds of a 95% CI for leakage rate (the number of pest leakage events in
#'   a random year).
#' @param establishment_rate Numeric vector of 2 values, giving the lower and
#'   upper bounds of a 95% CI for establishment rate (the rate of survival, to
#'   establishment, for leakage events).
#' @param outfile Character. Output raster file path. If `probability` has
#'   length > 1, the file type must support multiband rasters (e.g. GeoTiff). If
#'   not provided, the resulting [`SpatRaster`] object will be returned to R.
#' @param return_rast Logical. Should the [`SpatRaster`] object be returned to R?
#'   Ignored if `outfile` is not provided.
#' @param overwrite Logical. Should `outfile` be overwritten if it exists?
#'   Default is `FALSE`.
#' @return If `outfile` is specified, the resulting [`SpatRaster`] object
#'   (multiband if `probability` has length > 1) is saved as a GeoTiff at that
#'   path. If `return_rast` is `TRUE` or `outfile` is not specified, the
#'   resulting [`SpatRaster`] object is returned, otherwise `NULL` is returned
#'   invisibly.
#' @family functions estimating arrivals
#' @importFrom terra vect rast rasterize merge writeRaster
#' @importFrom methods is
#' @importFrom utils read.csv
#' @importFrom dplyr filter mutate
#' @export
arrivals_by_containers <- function(container_weights, port_data,
  template_raster, leakage_rate, establishment_rate, outfile,
  return_rast=FALSE, overwrite=FALSE) {

  # Load container weight data if path provided
  if(is.character(container_weights)) {
    container_weights <- terra::vect(container_weights)
  }

  container_weights$POA_CODE <- as.numeric(container_weights$POA_CODE)

  # Load port_data
  port_data <- utils::read.csv(port_data)

  # Load template_raster
  if(is.character(template_raster) || is(template_raster, 'Raster')) {
    template_raster <- terra::rast(template_raster)
  } else if(!is(template_raster, 'SpatRaster')) {
    stop('template_raster must be a SpatRaster or RasterLayer object, ',
         'or a file path to a raster file.')
  }

  # Subset port data to ports present in container weights
  port_data <- port_data %>%
    dplyr::filter(Name %in% names(container_weights)) %>%
    dplyr::mutate(Name = as.character(Name))

  # Multiply proportional postcode distribution volumes by port container
  # arrival volumes, and sum resulting postcode arrivals across source ports
  container_weights$total <-
    rowSums(sweep(as.data.frame(container_weights[, port_data$Name]), 2,
        port_data$Count, `*`))

  # Convert total postcode arrivals to a proportion of their sum
  container_weights$total <-
    container_weights$total/sum(container_weights$total, na.rm=TRUE)

  # COMMENT THIS
  postcode_rast <- container_weights %>%
    terra::rasterize(y = template_raster, field = "POA_CODE")

  postcode_n <- as.data.frame(postcode_rast) %>%
    na.omit %>%
    dplyr::group_by(POA_CODE) %>%
    dplyr::summarise(n_cells=dplyr::n())

  container_weights <- container_weights %>%
    terra::merge(postcode_n, by='POA_CODE')
  container_weights$total_per_cell <- container_weights$total/container_weights$n_cells

  # Rasterize postcode arrivals
  weight <- terra::rasterize(container_weights, y=template_raster,
                             field='total_per_cell')

  # Rescale due to missing postcodes not being rasterised (not overlaying cell
  # centres).
  weight <- calc_proportion(weight)

  # Calculate arrival rate
  EE <- calc_EE(leakage_rate, establishment_rate)
  out <- calc_pathway_pr(EE, weight, return_rast=TRUE)

  if(!missing(outfile)) {
    # Create directory if it does not exist
    if(!dir.exists(dirname(outfile))) {
      dir.create(dirname(outfile), recursive = TRUE)
    }

    # write out raster
    terra::writeRaster(out, outfile, overwrite = overwrite)
  }
  if(isTRUE(return_rast) || missing(outfile)) {
    out
  } else {
    invisible(NULL)
  }
}
