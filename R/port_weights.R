#' Create raster giving the establishment likelihoods relating to ports.
#'
#' Creates spatial port establishment likelihood raster as a function of port 
#' container volumes and distance from port.
#'
#' @param template_raster A \code{RasterLayer} or path to supported raster 
#'   file.
#' @param port_data Character. Path to csv file containing port container
#'   volumes and named Latitude and Longitude (in WGS84) for each port of 
#'   interest.
#' @param beta Numeric. The beta coefficient exponential function of how risk
#'   changes with distance from port. beta should be in units per 1km.
#' @param outfile Character. Output raster file path. Directory will be 
#'   created if it does not exist.If not provided, object will be returned to 
#'   R. 
#' @param return_rast Logical. Should the resulting raster be returned to R?
#'   Ignored if \code{outfile} is not provided.
#' @return If \code{outfile} is specified, the resulting raster is saved as a
#'   geotiff to that path. If \code{return_rast} is \code{TRUE} or
#'   \code{outfile} is not specified the resulting raster is returned, 
#'   otherwise \code{NULL} is returned invisibly.
#' @importFrom magrittr "%>%" divide_by multiply_by
#' @importFrom raster raster projection rasterToPoints cellFromXY writeRaster
#' @importFrom utils read.csv
#' @importFrom sf st_as_sf st_transform as_Spatial
#' @importFrom sp spDists
#' @export

port_weights <- function(template_raster, port_data, beta, outfile, 
                         return_rast=FALSE) {
  
  if(is.character(template_raster)) {
    template_raster <- raster::raster(template_raster)
  }
  
  aus_ports <- sf::st_as_sf(utils::read.csv(port_data), 
                            coords = c("Longitude", "Latitude"), crs = 4326) %>%
    sf::st_transform(crs = raster::projection(template_raster)) %>%
    sf::as_Spatial() 
  
  # Convert raster to points (faster than rasterToPoints - we don't need the 
  # data field)
  cells <- sp::SpatialPoints(
    raster::xyFromCell(template_raster, 
                       raster::Which(!is.na(template_raster), cell=TRUE)), 
    proj4string=raster::crs(template_raster))
    
  # Estimate distance from each port, apply distance x container weight &
  # Sum cell values return raster object
  if(interactive()) cat(sprintf('\r%.02f%%', 1/length(aus_ports)*100))
  # ^ progress indicator if interactive
  d <- sp::spDists(cells, aus_ports[1, ]) # first port, to initialise val vector
  val <- exp(d/(1000/beta))*aus_ports[["Count"]][1]
  # replacing rowSums(sapply, ...) with for loop for memory management
  for (i in seq_along(aus_ports)[-1]) { # subsequent ports
    cat(sprintf('\r%.02f%%', i/length(aus_ports)*100)) # progress indicator
    d <- sp::spDists(cells, aus_ports[i, ])  
    val <- val + exp(d/(1000/beta))*aus_ports[["Count"]][i]
  }
  out <- raster::raster(template_raster)
  out[raster::cellFromXY(out, cells)] <- val
  
  # convert to proportions
  out <- calc_proportion(out)
  
  if(!missing(outfile)) {
    if(!dir.exists(dirname(outfile))) {
      dir.create(dirname(outfile), recursive = TRUE)
    }
    # write out raster
    raster::writeRaster(out, outfile, overwrite = TRUE)
  }

  if(isTRUE(return_rast) || missing(outfile)) out else invisible(NULL)
}
