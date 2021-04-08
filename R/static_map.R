#' Produce a static map of establishment likelihood
#'
#' Produce a static map of establishment likelihood, with OpenStreetMap base
#' layer.
#'
#' @param ras A \code{Raster*} object or file path to a (potentially multiband)
#'   raster file.
#' @param xlim Numeric vector. The longitudinal extent of the area to plot.
#' @param ylim Numeric vector. The latitudinal extent of the area to plot.
#' @param layer Character. A layer name to be plotted. Only relevant if loading
#'   a \code{RasterStack} or \code{RasterBrick}.
#' @param layer_names Optional panel titles names for multipanel maps. If not
#'   provided, panels will not be given titles.
#' @param legend_title Character. Legend title.
#' @param set_value_range A numeric vector containing an upper and lower bound
#'   value (in units of raster). Values outside this range (including values
#'   falling on the boundaries) will be masked.
#' @param scale_type Character. Can be: \code{"none"} (raw data, no rescaling),
#'   \code{"log10"}, \code{"max normalize"} (proportional to maximum value),
#'   \code{"minmax normalize"} (rescale values to be between zero and 1 based
#'   on min and max), or \code{"logit"}. Note that if \code{"log10"} or
#'   \code{"logit"} is used 0 or 1 values. must be masked (using
#'   \code{set_value_range}) or rescaled outside of this function.
#' @param basemap_mode Either `'boundaries'` or `'osm'` (i.e., OpenStreetMap),
#'   defining whether OpenStreetMap imagery should be used for static map
#'   basemaps, or simple administrative boundaries. Default is `'osm'`.
#' @param transparency Numeric. Transparency of raster, between 0-1.
#' @param colramp_entire_range Logical. Whether to set colour ramp limits based
#'   on national risk range (\code{TRUE}) or by risk range present in region
#'   specified by \code{xlim} and \code{ylim}.
#' @param surveillance_locs A spatial object or a path to a .csv file
#'   containing columns named "Latitude" and "Longitude".
#' @param pt_col Character. The plotting colour for surveillance points.
#' @param aggregate_raster \code{NULL} or a list containing the aggregation
#'   factor (i.e. number of raster cells to aggregate) and the aggregation
#'   function e.g. \code{list(10, sum)}.
#' @param nrow For multipanel plots, an optional  numeric value defining the
#'   number of rows of panels.
#' @param height height of plot in inches (will be rendered at 300 dpi). If not
#'   defined will use size of current graphic device. Width will be determined
#'   automatically, based on the aspect ratio given by the plotting extent.
#' @param outfile Character. File path to save map.
#' @return If \code{outfile} is provided, a map is saved to that file.
#'   Otherwise, a \code{tmap} object is returned to R.
#' @details This function relies on the \code{OpenStreetMap} package to obtain
#'   base layer tiles. This in turn requires Java to be installed, and linking
#'   R with Java can sometimes prove challenging. On macOS, Java errors can
#'   often be resolved by entering \code{sudo R CMD javareconf} in a terminal (
#'   which updates R's Java configuration variables). On Windows, ensure that
#'   the Java architecture (32-bit/64-bit) matches that of R. Additionally,
#'   some Java errors arise when using RStudio but not when using R.
#' @importFrom dplyr filter
#' @importFrom gdalUtilities gdalwarp
#' @importFrom raster aggregate extent maxValue minValue ncell projectRaster setMinMax stack writeRaster
#' @importFrom sf st_as_sf st_crop st_crs st_read st_transform
#' @importFrom stats qlogis
#' @importFrom tmap tm_compass tm_dots tm_facets tm_layout tm_polygons tm_raster tm_rgb tm_scale_bar tm_shape tmap_mode tmap_options tmap_save
#' @importFrom tmaptools bb read_osm
#' @importFrom utils read.csv
#' @importFrom magrittr "%>%"
#' @export

static_map <- function(ras, xlim, ylim, layer, layer_names, legend_title,
                       set_value_range, scale_type = "none",
                       basemap_mode=c('osm', 'boundaries'),
                       transparency = 0.7, colramp_entire_range = TRUE,
                       surveillance_locs, pt_col = "red", aggregate_raster,
                       nrow, height, outfile) {

  if(missing(height)) {
    stop('height must be specified.')
  }

  basemap_mode <- match.arg(basemap_mode)

  # If an incorrect scale_type is specified
  scale_type <- match.arg(scale_type, c(
    "none", "log10", "max normalize", "minmax normalize", "logit",
    "discrete"))

  if(is.character(ras)) ras <- raster::stack(ras)

  if(missing(layer_names)) {
    layer_names <- rep('', dim(ras)[3])
  } else if(length(layer_names) != dim(ras)[3]) {
    stop('layer_names has invalid length')
  }

  ras <- raster::setMinMax(ras)

  e <- tmaptools::bb(xlim=xlim, ylim=ylim)

  if(basemap_mode=='osm') {
    basemap <- tmaptools::read_osm(e, zoom=NULL)
    basemap_res <- c(abs(attr(basemap, 'dimensions')$x$delta),
                      abs(attr(basemap, 'dimensions')$y$delta))
  } else {
    basemap <- sf::st_read(system.file('extdata/ne_australia_states.gpkg',
                                       package='edmaps')) %>%
      sf::st_crop(e)
  }

  # Aggregate raster (if required)
  if(!missing(aggregate_raster)) {
    ras <- raster::aggregate(ras, fact = aggregate_raster[[1]],
                             fun = aggregate_raster[[2]])
    ras[is.nan(ras)] <- NA
  }

  # Restrict Raster value range?
  if(!missing(set_value_range)) {
    ras[ras <= min(set_value_range) | ras >= max(set_value_range)] <- NA
  }

  # if the raster contains non-zero values...
  if(!is.na(raster::minValue(ras))) {
    # Convert to log10 scale
    if(scale_type == "log10") {
      if(any(raster::minValue(ras)) <= 0) {
        stop('Cannot log transform raster containing zero or negative values.')
      } else {
        ras <- log10(ras)
      }
    }

    # Max normalize
    if(scale_type == "max normalize") {
      ras <- max_normalize(ras)
    }
    # Min Max normalize
    if(scale_type == "minmax normalize") {
      ras <- min_max_normalize(ras)
    }

    # Convert to logit scale
    if(scale_type == "logit") {
      if(raster::minValue(ras) <= 0 | raster::maxValue(ras) >= 1) {
        stop('Cannot logit transform raster containing values <= 0 or >= 1.')
      } else {
        ras <-  stats::qlogis(ras)
      }
    }
  }

  full_range <- c(min(raster::minValue(ras), na.rm=TRUE),
                  max(raster::maxValue(ras), na.rm=TRUE))

  # Reproject raster to web mercator
  # https://github.com/mtennekes/tmap/issues/410
  # https://github.com/mtennekes/tmap/issues/412
  if(scale_type == "discrete") {
    raster::projectRaster(
      ras, crs = "+init=epsg:3857",
      res = if(basemap_mode=='osm') basemap_res else c(5000, 5000),
      method = "ngb", te=e, te_srs='EPSG:4283')
  } else {
    raster::writeRaster(ras, f <- tempfile(fileext='.tif'))
    gdalUtilities::gdalwarp(
      f, f2 <- tempfile(fileext='.tif'),
      t_srs = "EPSG:3857",
      tr = if(basemap_mode=='osm') basemap_res else c(5000, 5000),
      r = "bilinear", te=e, te_srs='EPSG:4283')
      # ^ This will interpolate the aggregated data if
      #   aggregate_raster is not NULL
    ras <- raster::setMinMax(raster::stack(f2))
  }

  if(isTRUE(colramp_entire_range)) {
    limit_cols <- full_range
  } else {
    limit_cols <- c(raster::minValue(ras), raster::maxValue(ras))
  }

  # Set tmap options
  if(basemap_mode=='osm') {
    opts <- tmap::tmap_options(
      max.raster=c(plot=max(raster::ncell(basemap), raster::ncell(ras)),
                   view=max(raster::ncell(basemap), raster::ncell(ras))),
      overlays = c(Labels = paste0(
        "http://services.arcgisonline.com/arcgis/rest/services/Canvas/",
        "World_Light_Gray_Reference/MapServer/tile/{z}/{y}/{x}"))
    )
  } else {
    opts <- tmap::tmap_options(
      max.raster=c(plot=raster::ncell(ras), view=raster::ncell(ras)),
      overlays = c(Labels = paste0(
        "http://services.arcgisonline.com/arcgis/rest/services/Canvas/",
        "World_Light_Gray_Reference/MapServer/tile/{z}/{y}/{x}"))
    )
  }

  suppressMessages(tmode <- tmap::tmap_mode('plot'))
  on.exit({
    tmap::tmap_options(opts)
    suppressMessages(tmap::tmap_mode(tmode))
  })

  minval <- raster::minValue(ras)

  m <- if(basemap_mode=='osm') {
    tmap::tm_shape(basemap) +
      tmap::tm_rgb()
  } else {
    tmap::tm_shape(basemap) +
      tmap::tm_polygons(col='white')
  }

  if(all(is.na(minval))) {
    m <- m +
      tmap::tm_shape(ras) +
      tmap::tm_raster(style='cont', midpoint=NA,
                      title=legend_title,
                      breaks=pretty(limit_cols, n=7),
                      alpha=transparency,
                      legend.is.portrait=FALSE) +
      tmap::tm_scale_bar(position=c('left', 'bottom'), text.size=0.75) +
      tmap::tm_compass() +
      tmap::tm_layout(outer.margins=c(0, 0, 0, 0),
                      legend.position=c('left', 'bottom'),
                      inner.margin=c(1/height, 0, 0, 0),
                      legend.text.size=0.8)
  } else {
    m <- m +
      tmap::tm_shape(ras) +
      tmap::tm_raster(palette='inferno', style='cont', midpoint=NA,
                      title=legend_title,
                      breaks=pretty(limit_cols, n=7),
                      alpha=transparency,
                      legend.is.portrait=FALSE) +
      tmap::tm_scale_bar(position=c('left', 'bottom'), text.size=0.75) +
      tmap::tm_compass() +
      tmap::tm_layout(outer.margins=c(0, 0, 0, 0),
                      legend.position=c('left', 'bottom'),
                      inner.margin=c(1/height, 0, 0, 0),
                      legend.text.size=0.8)
  }

  if(!all(layer_names == '')) m <- m + tmap::tm_layout(panel.labels=layer_names)
  m <- m + tmap::tm_layout(legend.outside.position='bottom')
  if(!missing(nrow)) m <- m + tmap::tm_facets(nrow=nrow)

  # Add surveillance locations (if required)
  if(!missing(surveillance_locs)) {
    if(is.character(surveillance_locs)) {
      surveillance_locs <- utils::read.csv(surveillance_locs) %>%
        dplyr::filter(!is.na(Longitude), !is.na(Latitude))
    }

    locs <- suppressWarnings(suppressMessages(
      sf::st_as_sf(surveillance_locs, coords = c("Longitude", "Latitude"),
                   crs = 4326) %>%
      sf::st_transform(crs = sf::st_crs(ras)) %>%
      sf::st_crop(y = raster::extent(ras))
    ))
    m <- m +
      tmap::tm_shape(locs) +
      tmap::tm_dots(col = pt_col, shape=21, size=0.25)
  }

  # outfile supplied
  if(!missing(outfile)) {
    # Create directory if it does not exist
    if(!dir.exists(dirname(outfile))) {
      dir.create(dirname(outfile), recursive = TRUE)
    }
    tmap::tmap_save(m, filename = outfile, height=height*300)
  } else {
    m
  }
}
