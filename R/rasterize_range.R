#' Rasterize species range
#'
#' Rasterize a species' range, either by burning points directly into raster, or
#' by calculating an alpha hull around points and burning the hull into raster.
#'
#' @param xy Coordinates to define host/species' range. This must be one of: a
#'   matrix with two columns giving longitude and latitude (in that order); a
#'   file path to a csv file that contains columns "Longitude" and "Latitude";
#'   a `SpatialPoints` object; or a `sf` multipoints object. Points
#'   will be reduced to the subset that falls within the `template` extent.
#' @param method Either `points` to burn `xy` points into raster, or
#'   `alphahull` to calculate the alpha hull of `xy`, and burn those
#'   polygons into raster.
#' @param alpha Alpha parameter for alpha hull calculation. Ignored if
#'   `method` is `'points'`.
#' @param point_buffer When `method = "points"`, the width of a buffer to
#'   generate around `xy` points before burning into the raster. Expected
#'   to be in the CRS of `template`. Ignored if `method` is
#'   `'alphahull'`.
#' @param template `RasterLayer` or file path to raster file. The host
#'   raster resulting from this function will use the extent and resolution of
#'   this template.
#' @param outfile Optional file path to write out resulting host raster.
#' @param xy_crs Coordinate reference system of `xy`, passed as numeric
#'   EPSG code, or any other format accepted by [sf::st_set_crs()]. If
#'   missing and `xy` is an `sp` or `sf` object, the CRS is
#'   defined by the object. If the latter is undefined, or if `xy_crs` is
#'   missing and `xy` is a matrix or path to a csv file, `xy` will
#'   inherit the CRS of `template`, and if that is undefined, EPSG:4326
#'   (WGS84) will be assumed. If `xy_crs` is provided and `xy` is an
#'   `sf` or `sp` object with a defined CRS, `xy_crs` will be
#'   ignored.
#' @param plot Logical. Plot the resulting raster? Default = `TRUE`.
#' @return A `RasterLayer` with the resulting range burnt into it.
#'   Additionally, if `outfile` is not missing, the raster is written to
#'   that file.
#' @importFrom dplyr select
#' @importFrom fasterize fasterize
#' @importFrom raster cellFromXY init raster writeRaster res plot
#' @importFrom readr read_csv
#' @importFrom sf st_coordinates st_crs st_multipoint st_set_crs st_sfc st_transform st_buffer st_as_sf st_bbox
#' @importFrom sp coordinates proj4string
#' @importFrom tmaptools read_osm
#' @importFrom tmap tm_shape tm_raster tm_rgb tm_dots tm_facets tm_legend
#' @export
rasterize_range <- function(xy, method, alpha, point_buffer=0, template, outfile,
                            xy_crs, plot=TRUE) {
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
    sf::st_intersection(sf::st_as_sfc(sf::st_bbox(template)))
  host <- raster::init(template, function(x) NA)
  method <- match.arg(method, c('points', 'alphahull'))
  if(method=='points') {
    if(point_buffer > 0) {
      xy_buffer <- sf::st_buffer(sf::st_as_sf(xy), point_buffer)
      host <- fasterize::fasterize(xy_buffer, host)
    } else {
      host[raster::cellFromXY(host, sf::st_coordinates(xy)[, 1:2])] <- 1
    }
  } else if(method=='alphahull') {
    host_poly <- alphahull_sf(sf::st_coordinates(xy)[, 1:2], alpha,
                              buffer_width=raster::res(template)[1]/100)
    host <- fasterize::fasterize(host_poly, host)
  }
  if(!missing(outfile)) {
    raster::writeRaster(host, outfile)
  }
  if(isTRUE(plot)) {
    e <- sf::st_bbox(host)
    basemap <- tmaptools::read_osm(e, zoom=NULL)
    m <- tmap::tm_shape(basemap) +
      tmap::tm_rgb() +
      tmap::tm_shape(host) +
      tmap::tm_raster(palette ="red", alpha = 0.5) +
      tmap::tm_shape(xy) +
      tmap::tm_dots(size = 0.01) +
      tmap::tm_legend(show=FALSE)
    print(m)
  }
  host
}
