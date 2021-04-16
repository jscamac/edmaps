#' Rasterize species range
#'
#' Rasterize a species' range, either by burning points directly into raster, or
#' by calculating an alpha hull around points and burning the hull into raster.
#'
#' @param xy Coordinates to define host/species' range. This must be one of: a
#'   matrix with two columns giving longitude and latitude (in that order); a
#'   file path to a csv file that contains columns "Longitude" and "Latitude";
#'   a \code{SpatialPoints} object; or a \code{sf} multipoints object. Points
#'   will be reduced to the subset that falls within the \code{template} extent.
#' @param method Either \code{points} to burn \code{xy} points into raster, or
#'   \code{alphahull} to calculate the alpha hull of \code{xy}, and burn those
#'   polygons into raster.
#' @param alpha Alpha parameter for alpha hull calculation. Ignored if
#'   \code{method} is \code{'points'}.
#' @param template \code{RasterLayer} or file path to raster file. The host
#'   raster resulting from this function will use the extent and resolution of
#'   this template.
#' @param outfile Optional file path to write out resulting host raster.
#' @param xy_crs Coordinate reference system of \code{xy}, passed as numeric
#'   EPSG code, or any other format accepted by \code{\link{sf::st_set_crs}}. If
#'   missing and \code{xy} is an \code{sp} or \code{sf} object, the CRS is
#'   defined by the object. If the latter is undefined, or if \code{xy_crs} is
#'   missing and \code{xy} is a matrix or path to a csv file, \code{xy} will
#'   inherit the CRS of \code{template}, and if that is undefined, EPSG:4326
#'   (WGS84) will be assumed. If \code{xy_crs} is provided and \code{xy} is an
#'   \code{sf} or \code{sp} object with a defined CRS, \code{xy_crs} will be
#'   ignored.
#' @return A \code{RasterLayer} with the resulting range burnt into it.
#'   Additionally, if \code{outfile} is not missing, the raster is written to
#'   that file.
#' @importFrom dplyr select
#' @importFrom fasterize fasterize
#' @importFrom raster cellFromXY init raster writeRaster
#' @importFrom readr read_csv
#' @importFrom sf st_coordinates st_crs st_multipoint st_set_crs st_sfc st_transform
#' @importFrom sp coordinates proj4string
#' @export
rasterize_range <- function(xy, method, alpha, template, outfile, xy_crs) {
  if(is.character(template)) {
    template <- raster::raster(template)
  } else if(!is(template, 'RasterLayer')) {
    stop('template must be a RasterLayer or a file path to a raster file.')
  }
  r_crs <- sf::st_crs(template)
  if(is.character(xy)) {
    xy <- as.matrix(dplyr::select(readr::read_csv(xy), Longitude, Latitude))
    if(missing(xy_crs)) {
      if(is.na(r_crs)) {
        xy_crs <- 4326
        warning('CRS of xy assumed to be EPSG:4326 (WGS84)')
      } else {
        xy_crs <- r_crs
        warning('CRS of xy assumed to be ', r_crs)
      }
    }
  } else if(is.matrix(xy)) {
    xy <- xy[, 1:2]
    if(missing(xy_crs)) {
      if(is.na(r_crs)) {
        xy_crs <- 4326
        warning('CRS of xy assumed to be EPSG:4326 (WGS84)')
      } else {
        xy_crs <- r_crs
        warning('CRS of xy assumed to be ', r_crs)
      }
    }
  } else if(is(xy, 'SpatialPoints')) {
    if(is.na(sp::proj4string(xy)) && missing(xy_crs)) {
      if(is.na(r_crs)) {
        xy_crs <- 4326
        warning('CRS of xy assumed to be EPSG:4326 (WGS84)')
      } else {
        xy_crs <- r_crs
        warning('CRS of xy assumed to be ', r_crs)
      }
    } else if(!is.na(sp::proj4string(xy))) {
      xy_crs <- sp::proj4string(xy)
    }
    xy <- sp::coordinates(xy)[, 1:2]
  } else if(is(xy, 'sf')) {
    if(is.na(sf::st_crs(xy)) && missing(xy_crs)) {
      if(is.na(r_crs)) {
        xy_crs <- 4326
        warning('CRS of xy assumed to be EPSG:4326 (WGS84)')
      } else {
        xy_crs <- r_crs
        warning('CRS of xy assumed to be ', r_crs)
      }
    } else if(!is.na(sf::st_crs(xy))) {
      xy_crs <- sf::st_crs(xy)
    }
    xy <- sf::st_coordinates(xy)[, 1:2]
  } else {
    stop('xy must be one of: sf points object; SpatialPoints* object; matrix',
         ' containing Longitude and Latitude; or a file path to a csv',
         ' containing columns Longitude and Latitude.')
  }
  colnames(xy) <- c('Longitude', 'Latitude')
  xy <- sf::st_multipoint(unique(xy)) %>%
    sf::st_sfc() %>%
    sf::st_set_crs(xy_crs) %>%
    sf::st_transform(sf::st_crs(template)) %>%
    sf::st_intersection(sf::st_as_sfc(sf::st_bbox(template))) %>%
    sf::st_coordinates() %>%
    .[, 1:2]
  host <- raster::init(template, function(x) NA)
  method <- match.arg(method, c('points', 'alphahull'))
  if(method=='points') {
    host[raster::cellFromXY(host, xy)] <- 1
  } else if(method=='alphahull') {
    host_poly <- alphahull_sf(xy, alpha)
    host <- fasterize::fasterize(host_poly, host)
  }
  if(!missing(outfile)) {
    raster::writeRaster(host, outfile)
  }
  host
}
