#' Rasterize wind data
#'
#' Process and rasterize wind data, creating raster datasets that describe wind
#' speed within a specified distance of the coastline.
#'
#' @param data One of: A [`SpatVector`] object with polygon geometry, an `sf`
#'   object with polygon geometry, a `SpatialPolygons` object, or a file path to
#'   such a file. This object should describe onshore winds.
#' @param wind_column Character. The column name for the column of the object
#'   defined by `data` that contains wind speed data.
#' @param template A `RasterLayer` or [`SpatRaster`] object, or a character
#'   file path to a file that can be read by GDAL, defining the extent and
#'   resolution of analysis, and defining the coastline (edge of non-NA values).
#'   Must have a valid coordinate system. If missing, CRS is assumed to be
#'   Australian Albers (3577).
#' @param width Numeric. The width of the coastal buffer (in metres), defining
#'   how far inland the wind has an effect. E.g. if the pest is thought to be
#'   carried up to 50km inland by the wind, set this value to `50000`.
#' @param outfile Character. The target file path for the wind raster.
#' @param overwrite Logical. Should `outfile` be overwritten if it exists?
#'   Default is `FALSE`.
#' @importFrom terra aggregate as.points as.polygons buffer crs disagg erase
#'   expanse init intersect makeValid mask merge rast rasterize subset union
#'   values vect voronoi writeRaster
#' @importFrom utils tail
#' @importFrom magrittr %>%
#' @importFrom methods is
#' @export
wind_weight <- function(data, wind_column, template, width, outfile,
                           return_rast=FALSE, overwrite=FALSE) {

  if(is.character(data) || is(data, 'SpatialPolygons') || is(data, 'sf')) {
    data <- terra::vect(data)
  } else {
    if(!is(data, 'SpatVector')) {
      stop('data must be a `SpatVector`, `sf`, or `sp` object, or a file ',
           'path to such a dataset.', call.=FALSE)
    }
  }

  if(!wind_column %in% names(data)) {
    stop(sprintf('Column %s not found', wind_column))
  }

  # Read in JW's wind data, and resolve polygon overlap: for each wind
  # component, assign maximum of overlapping polygons in areas of overlap
  wind <- terra::union(data) # shows which polygon segments overlap
  # Where wind polygons overlap, choose the one with the highest wind for the
  # chosen wind vector.
  wind$segment_id <- sapply(seq_along(wind), function(i) {
    origins <- which(terra::values(wind)[i, ]==1)
    j <- origins[which.max(data[[wind_column]][, 1][origins])]
    data$id[j]
  })
  # Merge in wind data
  wind <- terra::merge(wind[, 'segment_id'], data, by.x='segment_id', by.y='id')
  # Copy wind[[wind_column]] to wind$wind for convenience
  wind$wind <- wind[[wind_column]][, 1]
  wind <- terra::subset(wind, wind$wind > 0, select=c('segment_id', 'wind'))
  wind <- wind[order(wind$segment_id), ]
  wind <- terra::aggregate(wind, 'segment_id', count=FALSE) %>%
    setNames(c('segment_id', 'wind'))

  # Buffer JW's wind polygons to identify corresponding segments of coastline.
  # We buffer the first and last polygon, as well as polygon O, using a smaller
  # width to avoid including unwanted regions of coastline.
  # NB: width (inland wind effect width) is a function arg, so the hardcoded
  # buffers below will be insufficient if user passes a large width.
  wind_buffer <- rbind(
    terra::buffer(terra::subset(
      wind, wind$segment_id %in% c(wind$segment_id[1], 'O', tail(wind$segment_id, 1))),
      100000
    ),
    terra::buffer(terra::subset(
      wind, !wind$segment_id %in% c(wind$segment_id[1], 'O', tail(wind$segment_id, 1))),
      300000
    )
  ) %>% terra::aggregate()

  # Create 1000 x 1000m template raster and convert to polygons
  if(is.character(template) || is(template, 'Raster')) {
    template <- terra::rast(template)
  } else if(!is(template, 'SpatRaster')) {
    stop('template must be a Raster* object, a SpatRaster object, ',
         'or a file path to a raster file.')
  }

  template <- template[[1]] # select first layer in case is multilayer

  # Assume 3577
  terra::crs(template) <- 'epsg:3577'
  names(template) <- 'template'

  # Derive vector shapes from template
  #template[] <- ifelse(is.na(terra::values(template)), NA, 1) #
  aus <- terra::mask(terra::init(template, 1), template) %>%
    terra::as.polygons(values=FALSE) %>%
    terra::makeValid() %>%
    terra::disagg()

  aus_shrink <- terra::buffer(aus, -width)
  aus_shrink <- aus_shrink[which(terra::expanse(aus_shrink) > 0)]
  b <- terra::intersect(terra::erase(aus, aus_shrink), wind_buffer)

  # Convert polygons to grids of points. Note that simply converting the
  # boundary lines to points with as.points can lead to problems (esp. where
  # wind polygons overlap).
  wind2 <- terra::rasterize(wind, terra::rast(wind, res=5000)) %>%
    terra::as.points() %>%
    terra::intersect(wind) %>%
    .[, c('segment_id', 'wind')]

  # Voronoi tessellation to find nearest wind poly for all areas of coastal
  # buffer.
  v <- terra::aggregate(terra::voronoi(wind2), 'segment_id', count=FALSE) %>%
    setNames(c('segment_id', 'wind'))

  wind_matched <- terra::intersect(v, b) %>%
    terra::subset(wind > 0, NSE=TRUE)

  # Rasterise result
  weight <- terra::rasterize(wind_matched, init(template, NA), field='wind')

  if(!missing(outfile)) {
    # Create directory if it does not exist
    if(!dir.exists(dirname(outfile))) {
      dir.create(dirname(outfile), recursive=TRUE)
    }
    terra::writeRaster(weight, outfile, overwrite=TRUE)
  }

  if(isTRUE(return_rast) || missing(outfile)) weight else invisible(outfile)
}
