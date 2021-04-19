#' Rasterize wind data
#'
#' Process and rasterize wind data, creating raster datasets that describe wind
#' speed within a specified distance of the coastline.
#'
#' @param data Character. A vector polygon dataset (e.g. Shapefile or
#'   GeoPackage) describing onshore winds, or a file path to such a file.
#' @param wind_column Character. The column name for the column of the object
#'   defined by \code{data} that contains wind speed data.
#' @param template A \code{RasterLayer} or \code{stars} object, or a character
#'   file path to a file that can be read by GDAL, defining the extent and
#'   resolution of analysis, and defining the coastline (edge of non-NA values).
#'   If a \code{stars} object, the first attribute will be used. Must have a
#'   valid coordinate system. CRS is assumed to be Australian Albers (3577).
#' @param width Numeric. The width of the coastal buffer (in metres), defining
#'   how far inland the wind has an effect. E.g. if the pest is thought to be
#'   carried up to 50km inland by the wind, set this value to \code{50000}.
#' @param outfile Character. The target file path for the wind raster.
#' @importFrom dplyr filter group_by left_join mutate select summarise
#' @importFrom sf st_as_sf st_boundary st_buffer st_cast st_collection_extract st_coordinates st_geometry_type st_intersection st_join st_make_valid st_nearest_feature st_set_crs st_sf st_union st_voronoi st_drop_geometry
#' @importFrom stars read_stars st_rasterize write_stars st_as_stars
#' @importFrom stplanr rnet_get_nodes
#' @importFrom magrittr '%>%'
#' @export
rasterize_wind <- function(data, wind_column, template, width, outfile) {

  if(is.character(data)) {
    data <- sf::read_sf(data)
  } else {
    if(!is(data, 'sf')) {
      stop('sf must be a vector polygon dataset, or a file path to such a',
           ' dataset.')
    }
  }

  if(!wind_column %in% colnames(data))
    stop(sprintf('Column %s not found', wind_column))

  # Read in JW's wind data, and resolve polygon overlap: for each wind
  # component, assign maximum of overlapping polygons in areas of overlap
  wind <- data %>%
    sf::st_intersection() %>%
    dplyr::mutate(
      segment_id=sapply(origins, function(o) {
        i <- o[which.max(data[[wind_column]][o])]
        data[['id']][i]
      }),
      wind=data[[wind_column]][match(segment_id, data[['id']])]
    ) %>%
    dplyr::select(segment_id, wind) %>%
    dplyr::filter(wind > 0)


  # Buffer JW's wind polygons to identify corresponding segments of coastline.
  # We buffer the first and last polygon, as well as polygon O, using a smaller
  # width to avoid including unwanted regions of coastline.
  wind_buffer <- rbind(
    sf::st_buffer(dplyr::filter(
      wind, segment_id %in% c(segment_id[1], 'O', tail(segment_id, 1))), 50000),
    sf::st_buffer(dplyr::filter(
      wind, !segment_id %in% c(segment_id[1], 'O', tail(segment_id, 1))), 200000)
  ) %>% sf::st_union()

  # Create 1000 x 1000m template raster and convert to polygons
  if(is.character(template)) {
    template <- stars::read_stars(template)[1]
  } else if(is(template, 'RasterLayer')) {
    template <- stars::st_as_stars(template)[1]
  } else if(!is(template, 'stars')) {
    stop('template must be a stars object, a RasterLayer, or a file path to a',
         ' raster file.')
  }

  # Assume 3577
  template <- sf::st_set_crs(template, 3577)
  names(template) <- 'template'

  # Derive vector shapes from template
  aus <- template %>%
    dplyr::mutate(template=ifelse(is.na(template), NA, 1)) %>%
    sf::st_as_sf(merge=TRUE, connect8=F) %>%
    sf::st_make_valid() %>%
    dplyr::summarise(.groups='drop')
  aus_boundary <- sf::st_boundary(aus)
  aus_subset_boundary <- sf::st_intersection(aus_boundary, wind_buffer) %>%
    sf::st_cast('LINESTRING')

  ###  Find closest wind poly id for each segment
  # https://gis.stackexchange.com/a/358217/1249
  p <- sf::st_cast(aus_subset_boundary, "POINT") %>%
    dplyr::mutate(segment_id=wind$segment_id[sf::st_nearest_feature(., wind)])
  b <- sf::st_buffer(aus_subset_boundary, dist=width) %>%
    sf::st_union() %>%
    sf::st_intersection(aus) %>%
    sf::st_intersection(wind_buffer)
  junctions <- stplanr::rnet_get_nodes(aus_subset_boundary)
  sel_in_junctions <- paste(sf::st_coordinates(p$geometry)[, 1],
                            sf::st_coordinates(p$geometry)[, 2]) %in%
    paste(sf::st_coordinates(junctions)[, 1], sf::st_coordinates(junctions)[, 2])
  p_not_junctions <- p[!sel_in_junctions, ]
  v <- sf::st_voronoi(do.call("c", p_not_junctions$geometry), envelope=b) %>%
    sf::st_collection_extract('POLYGON')
  v_joined <- sf::st_join(sf::st_set_crs(sf::st_sf(v), 3577), p_not_junctions) %>%
    dplyr::group_by(segment_id) %>%
    dplyr::summarise(.groups='drop')
  v_intersection <- sf::st_intersection(v_joined, b)

  wind_matched <- v_intersection %>%
    dplyr::left_join(sf::st_drop_geometry(wind), by='segment_id') %>%
    dplyr::select(segment_id, wind) %>%
    dplyr::group_by(segment_id, wind) %>%
    dplyr::summarise(.groups='drop') %>%
    dplyr::filter(wind > 0)

  if(any(!sf::st_geometry_type(wind_matched) %in%
         c('POLYGON', 'MULTIPOLYGON'))) {
    wind_matched <- sf::st_collection_extract(wind_matched, 'POLYGON')
  }

  # Rasterize
  template_empty <- dplyr::mutate(template, template=NA)
  r <- stars::st_rasterize(wind_matched[, 'wind'], template_empty)

  # Write to raster file
  if(!missing(outfile)) {
    stars::write_stars(r, outfile)
  }

  r
}
