#' Estimates pest arrivals by containers
#'
#' Estimates pest arrivals by containers.
#'
#' @param container_weights An \code{\link{sf}} object or file path to a
#'   shapefile supported by OGR, as produced by
#'   \code{\link{container_weights}}.
#' @param port_data Character. Path to csv file containing Port Names, Port
#'   Codes, Longitude, Latitude and Container volumes.
#' @param template_raster \code{RasterLayer} or file path to a raster file.
#'   This is used to define the extent and resolution of output. Must be in
#'   CRS EPSG:3577.
#' @param probability Numeric. The probability that a unit carries the pest.
#' @param outfile Character. Output raster file path. If not provided, raster 
#'   object will be returned to R.
#' @param return_rast Logical. Should the raster object be returned to R?
#'   Ignored if \code{outfile} is not provided.
#' @return If \code{outfile} is specified, the resulting raster is saved as a
#'   to that path. If \code{return_rast} is \code{TRUE} or \code{outfile} is 
#'   not specified the resulting raster is returned, otherwise \code{NULL} is 
#'   returned invisibly.
#' @family functions estimating arrivals
#' @importFrom sf read_sf
#' @importFrom raster raster stack writeRaster
#' @importFrom utils read.csv
#' @importFrom dplyr filter mutate
#' @importFrom fasterize fasterize
#' @export
#'
arrivals_by_containers <- function(container_weights, port_data, 
  template_raster, probability, outfile, return_rast=FALSE) {

  # Load container weight data if path provided
  if(is.character(container_weights)) {
    container_weights <- sf::read_sf(container_weights)
  }
  
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
  
  # Rasterize and estimate number of pest arrivals per grid cell
  out <- sum(raster::stack(sapply(port_data$Name, function(x) {
    calc_proportion(fasterize::fasterize(
      sf = container_weights, raster = template_raster, field = x)) *
      port_data[port_data$Name==x, 5] #todo use column name
  }))) * probability

  if(!missing(outfile)) {
    # Create directory if it does not exist
    if(!dir.exists(dirname(outfile))) {
      dir.create(dirname(outfile), recursive = TRUE)
    }

    # write out raster
    raster::writeRaster(out, outfile, overwrite=TRUE)
  }
  if(isTRUE(return_rast) || missing(outfile)) {
    out
  } else {
    invisible(NULL)
  }
}
