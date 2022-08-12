#' Create raster giving the establishment likelihoods relating to ports.
#'
#' Creates spatial port establishment likelihood raster as a function of port
#' container volumes and distance from port.
#'
#' @param template_raster A `RasterLayer`, [`SpatRaster`], or path to supported
#'   raster file.
#' @param port_data Character. Path to csv file containing port container
#'   volumes and named Latitude and Longitude (in WGS84) for each port of
#'   interest.
#' @param beta Numeric. The beta coefficient exponential function of how risk
#'   changes with distance from port. beta should be in units per 1km.
#' @param outfile Character. Output raster file path. Directory will be created
#'   if it does not exist.If not provided, object will be returned to R.
#' @param return_rast Logical. Should the resulting raster be returned to R?
#'   Ignored if `outfile` is not provided.
#' @return If `outfile` is specified, the resulting [`SpatRaster`] is saved as a
#'   Geotiff to that path. If `return_rast` is `TRUE` or `outfile` is not
#'   specified the resulting [`SpatRaster`] is returned, otherwise `NULL` is
#'   returned invisibly.
#' @importFrom magrittr %>%
#' @importFrom terra rast crs cellFromXY cells writeRaster xyFromCell vect project crds distance init
#' @importFrom utils read.csv
#' @importFrom methods is
#' @export
port_weights <- function(template_raster, port_data, beta, outfile,
                         return_rast=FALSE) {

  if(is.character(template_raster) || is(template_raster, 'RasterLayer')) {
    template_raster <- terra::rast(template_raster)
  } else if(!is(template_raster, 'SpatRaster') || dim(template_raster)[3] > 1) {
    stop('template_raster must be a RasterLayer, single-layer SpatRaster, ',
         'or a single path to a raster file.', call.=FALSE)
  }

  aus_ports <- utils::read.csv(port_data) %>%
    terra::vect(geom=c("Longitude", "Latitude"), crs='+init=epsg:4326') %>%
    terra::project(terra::crs(template_raster))

  # Convert raster to points
  cells <- template_raster %>%
    terra::xyFromCell(terra::cells(!is.na(template_raster), 1)[[1]]) %>%
    terra::vect(crs=terra::crs(template_raster))

  # Estimate distance from each port, apply distance x container weight &
  # Sum cell values return raster object
  if(interactive()) cat(sprintf('\r%.02f%%', 1/length(aus_ports)*100))
  # ^ progress indicator if interactive
  d <- terra::distance(cells, aus_ports[1, ]) # first port, to initialise val vector
  val <- exp(d/(1000/beta))*aus_ports$Count[1]
  # replacing rowSums(sapply, ...) with for loop for memory management
  for (i in seq_along(aus_ports)[-1]) { # subsequent ports
    cat(sprintf('\r%.02f%%', i/length(aus_ports)*100)) # progress indicator
    d <- terra::distance(cells, aus_ports[i, ])
    val <- val + exp(d/(1000/beta))*aus_ports$Count[i]
  }
  out <- terra::init(template_raster, NA)
  out[terra::cellFromXY(out, terra::crds(cells))] <- val

  # convert to proportions
  out <- calc_proportion(out)

  if(!missing(outfile)) {
    if(!dir.exists(dirname(outfile))) {
      dir.create(dirname(outfile), recursive = TRUE)
    }
    # write out raster
    terra::writeRaster(out, outfile, overwrite = TRUE)
  }

  if(isTRUE(return_rast) || missing(outfile)) out else invisible(NULL)
}
