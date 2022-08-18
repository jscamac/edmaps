#' Rasterize species range
#'
#' Rasterize a species' range, either by burning points directly into raster, or
#' by calculating an alpha hull around points and burning the hull into raster.
#'
#' @param xy Coordinates to define host/species' range. This must be one of: a
#'   matrix with two columns giving longitude and latitude (in that order); a
#'   file path to a csv file that contains columns "Longitude" and "Latitude"; a
#'   [`SpatVector`] with points geom, a SpatialPoints` object; or a `sf`
#'   multipoints object. Points will be reduced to the subset that falls within
#'   the `template` extent.
#' @param method Either `points` to burn `xy` points into raster, or `alphahull`
#'   to calculate the alpha hull of `xy`, and burn those polygons into raster.
#' @param alpha Alpha parameter for alpha hull calculation. Ignored if `method`
#'   is `'points'`.
#' @param point_buffer When `method = "points"`, the width of a buffer to
#'   generate around `xy` points before burning into the raster. Expected to be
#'   in the CRS of `template`. Ignored if `method` is `'alphahull'`.
#' @param template `Raster*`, [`SpatRaster`], or file path to a raster file. The
#'   host raster resulting from this function will use the extent and resolution
#'   of this template.
#' @param outfile Optional file path to write out resulting host raster.
#' @param xy_crs Coordinate reference system of `xy`, passed as
#'   <authority:number> (e.g. `"EPSG:4326"`), or any other format accepted by
#'   [terra::crs()]. If missing and `xy` is an `sp`, `sf`, or [`SpatVector`]
#'   object, the CRS is defined by the object. If the latter is undefined, or if
#'   `xy_crs` is missing and `xy` is a matrix or path to a csv file, `xy` will
#'   inherit the CRS of `template`, and if that is undefined, EPSG:4326 (WGS84)
#'   will be assumed. If `xy_crs` is provided and `xy` is an `sf`, `sp`, or
#'   [`SpatVector`] object with a defined CRS, `xy_crs` will be ignored.
#' @param plot Logical. Plot the resulting raster? Default = `TRUE`.
#' @return A [`Spatraster`] with the resulting range burnt into it.
#'   Additionally, if `outfile` is not missing, the raster is written to that
#'   file.
#' @importFrom dplyr select
#' @importFrom readr read_csv
#' @importFrom terra as.polygons buffer cellFromXY crds crs ext init intersect project rast rasterize vect writeRaster
#' @importFrom tmap tm_dots tm_legend tm_raster tm_rgb tm_shape
#' @importFrom tmaptools read_osm
#' @export
rasterize_range <- function(xy, method, alpha, point_buffer=0, template, outfile,
                            xy_crs, plot=TRUE) {

  if(is.character(template) || is(template, 'RasterLayer')) {
    template <- terra::rast(template)
  } else if(!is(template, 'SpatRaster') || dim(template)[3] > 1) {
    stop('template must be a RasterLayer, single-layer SpatRaster, ',
         'or a single path to a raster file.', call.=FALSE)
  }

  r_crs <- terra::crs(template)
  if(is.character(xy)) {
    xy <- readr::read_csv(xy) %>%
      dplyr::select(Longitude, Latitude) %>%
      as.matrix()
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
    if(is.na(terra::crs(xy)) && missing(xy_crs)) {
      if(is.na(r_crs)) {
        xy_crs <- 4326
        warning('CRS of xy assumed to be EPSG:4326 (WGS84)')
      } else {
        xy_crs <- r_crs
        warning('CRS of xy assumed to be ', r_crs)
      }
    } else if(!is.na(terra::crs(xy))) {
      xy_crs <- terra::crs(xy)
    }
    xy <- terra::crds(xy)[, 1:2]
  } else if(is(xy, 'sf')) {
    if(is.na(terra::crs(xy)) && missing(xy_crs)) {
      if(is.na(r_crs)) {
        xy_crs <- 4326
        warning('CRS of xy assumed to be EPSG:4326 (WGS84)')
      } else {
        xy_crs <- r_crs
        warning('CRS of xy assumed to be ', r_crs)
      }
    } else if(!is.na(terra::crs(xy))) {
      xy_crs <- terra::crs(xy)
    }
    xy <- terra::crds(xy)[, 1:2]
  } else {
    stop('xy must be one of: sf points object; SpatialPoints* object; matrix',
         ' containing Longitude and Latitude; or a file path to a csv',
         ' containing columns Longitude and Latitude.')
  }
  colnames(xy) <- c('Longitude', 'Latitude')
  xy <- terra::vect(unique(xy), crs=xy_crs) %>%
    terra::project(terra::crs(template)) %>%
    terra::intersect(terra::as.polygons(terra::ext(template)))
  host <- terra::init(template[[1]], function(x) NA)
  method <- match.arg(method, c('points', 'alphahull'))
  if(method=='points') {
    if(point_buffer > 0) {
      xy_buffer <- terra::buffer(terra::vect(xy), point_buffer) # TODO: Verify this behaves similarly to st_buffer (check width)
      host <- terra::rasterize(xy_buffer, host)
    } else {
      host[terra::cellFromXY(host, terra::crds(xy)[, 1:2])] <- 1
    }
  } else if(method=='alphahull') {
    host_poly <- alphahull(terra::crds(xy)[, 1:2], alpha)
    host <- terra::rasterize(host_poly, host)
  }
  if(!missing(outfile)) {
    terra::writeRaster(host, outfile)
  }
  if(isTRUE(plot)) {
    e <- terra::ext(host)[c(1, 3, 2, 4)]
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
