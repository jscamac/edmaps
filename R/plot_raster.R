#' Plot a raster
#'
#' Plot a raster.
#' 
#' @param object A \code{RasterLayer} or file path to raster file.
#' @param legend_title Character. If missing, the name of the raster layer will
#'   be used.
#' @param occurrence_data A \code{data.frame}, \code{sf} object,
#'   \code{SpatialPointsDataFrame} object, or path to a .csv file containing
#'   columns named "Latitude" and "Longitude". If \code{NULL}, no points will 
#'   be plotted.
#' @param pt_col Character. Colour of points (if plotted).
#' @param height Height of plot in inches (will be rendered at 300 dpi).
#'   Required if \code{outfile} is provided.
#' @param compass Logical. Should a North arrow be shown?
#' @param outfile Character. Path to save output.
#' @return A \code{tmap} object. If \code{outfile} is provided, a map will also
#'   be written to that file.
#' @importFrom raster raster projection
#' @importFrom rnaturalearth ne_countries
#' @importFrom sf st_transform st_crop st_as_sf st_buffer
#' @importFrom tmap tm_shape tm_raster tm_polygons tm_dots tm_compass tm_layout tmap_save
#' @importFrom utils read.csv
#' @importFrom dplyr rename_all
#' @export

plot_raster <- function(object, legend_title, occurrence_data = NULL, 
  pt_col ="red", height, compass = FALSE, outfile) {
 
  if(!missing(outfile) & missing(height)) {
    stop('If outfile is provided, height must be specified.')
  }
  
  if(is.character(object)) {
    ras <- raster::raster(object)
  } else {
    ras <- object
  }
  
  if(missing(legend_title)) {
    legend_title <- names(ras)
  }
  
  world_map <- suppressMessages(suppressWarnings(
    rnaturalearth::ne_countries(scale = 50, returnclass = "sf") %>%
      sf::st_transform(crs = raster::projection(ras)) %>%
      sf::st_buffer(0) %>% # repair ring self-intersection in India poly
      sf::st_crop(sf::st_bbox(ras))
  ))
  
  m <- tmap::tm_shape(ras) +
    tmap::tm_raster(palette='inferno', style='cont', midpoint=NA, 
                    title=legend_title, 
                    breaks=seq(0, 1, 0.1), 
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
          sf::st_as_sf(coords = c("longitude","latitude"), 
                       crs = raster::projection(ras))
      )
    } else if("sf" %in% class(occurrence_data)) {
      occ <- suppressMessages(
        sf::st_transform(occurrence_data, crs = raster::projection(ras))
      )
    } else if("SpatialPointsDataFrame" %in% class(occurrence_data)) {
      occ <- suppressMessages(
        sf::st_as_sf(occurrence_data) %>%
          sf::st_transform(., crs = raster::projection(ras))
      )
    } else {
      occ <- suppressMessages(
        occurrence_data %>%
          dplyr::rename_all(tolower) %>%
          sf::st_as_sf(coords = c("longitude","latitude"),
                       crs = raster::projection(ras))
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
