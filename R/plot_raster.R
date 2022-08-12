#' Plot a raster
#'
#' Plot a raster.
#'
#' @param rast A `RasterLayer`, single-layer [`SpatRaster`] or file path to
#'   raster file.
#' @param legend_title Character. If missing, the name of the raster layer will
#'   be used.
#' @param occurrence_data A `data.frame`, `sf` object, `SpatialPointsDataFrame`
#'   object, [`SpatVector`] object, or path to a .csv file containing columns
#'   named "Latitude" and "Longitude". If `NULL`, no points will be plotted.
#' @param pt_col Character. Colour of points (if plotted).
#' @param height Height of plot in inches (will be rendered at 300 dpi).
#'   Required if `outfile` is provided.
#' @param compass Logical. Should a North arrow be shown?
#' @param outfile Character. Path to save output.
#' @return A `tmap` object. If `outfile` is provided, a map will also be written
#'   to that file.
#' @importFrom terra rast crs setMinMax minmax project crop vect buffer ext
#' @importFrom rnaturalearth ne_countries
#' @importFrom tmap tm_shape tm_raster tm_polygons tm_dots tm_compass tm_layout tmap_save tm_grid
#' @importFrom utils read.csv
#' @importFrom dplyr rename_all
#' @importFrom methods is
#' @export
plot_raster <- function(rast, legend_title, occurrence_data = NULL,
                        pt_col ="red", height, compass = FALSE, outfile) {

  if(!missing(outfile) & missing(height)) {
    stop('If outfile is provided, height must be specified.')
  }

  if(is.character(rast) || is(rast, 'RasterLayer')) {
    rast <- terra::rast(rast)
  } else if(!is(rast, 'SpatRaster') || dim(rast)[3] > 1) {
    stop('rast must be a RasterLayer, single-layer SpatRaster, ',
         'or a single path to a raster file.', call.=FALSE)
  }
  terra::setMinMax(rast)

  if(missing(legend_title)) {
    legend_title <- names(rast)
  }

  world_map <- suppressMessages(suppressWarnings(
    rnaturalearth::ne_countries(scale = 50, returnclass = "sf") %>%
      terra::vect() %>%
      terra::project(terra::crs(rast)) %>%
      terra::buffer(width=0) %>% # repair ring self-intersection in India poly
      terra::crop(terra::ext(rast))
  ))

  rng <- terra::minmax(rast)
  m <- tmap::tm_shape(rast) +
    tmap::tm_raster(palette='inferno', style='cont', midpoint=NA,
                    title=legend_title,
                    breaks=pretty(x = rng, n = 10, min.n = 5),
                    legend.is.portrait=FALSE) +
    tmap::tm_shape(world_map) +
    tmap::tm_polygons(alpha=0, border.col='black', border.alpha=1) +
    tmap::tm_layout(legend.position=c('left', 'bottom'),
                    inner.margin=c(1/height, 0, 0, 0),
                    legend.text.size=0.8) +
    tmap::tm_grid(lines=FALSE)

  if(isTRUE(compass)) {
    m <- m + tmap::tm_compass()
  }

  if(!is.null(occurrence_data)) {
    if(is.character(occurrence_data)) {
      occ <- suppressMessages(
        utils::read.csv(occurrence_data) %>%
          dplyr::rename_all(tolower) %>%
          terra::vect(geom=c("longitude","latitude"), crs=terra::crs(rast))
      )
    } else if(any(c('sf', 'SpatialPointsDataFrame') %in% class(occurrence_data))) {
      occ <- suppressMessages(
        terra::vect(occurrence_data) %>%
          terra::project(terra::crs(rast))
      )
    } else if(is(occurrence_data, 'SpatVector')) {
      occ <- terra::project(occurrence_data, terra::crs(rast))
    } else {
      occ <- suppressMessages(
        occurrence_data %>%
          dplyr::rename_all(tolower) %>%
          terra::vect(geom=c("longitude","latitude"), crs=terra::crs(rast))
      )
    }

    m <- m +
      tmap::tm_shape(occ) +
      tmap::tm_dots(col=pt_col, shape=3)
  }

  # outfile supplied
  if(!missing(outfile)) {
    # Create directory if it does not exist
    if(!dir.exists(dirname(outfile))) {
      dir.create(dirname(outfile), recursive = TRUE)
    }
    tmap::tmap_save(m, outfile, height=height*300, outer.margins=NA)
    m
  } else {
    m
  }
}
