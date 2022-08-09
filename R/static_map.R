#' Produce a static map of establishment likelihood
#'
#' Produce a static map of establishment likelihood, with OpenStreetMap base
#' layer.
#'
#' @param ras A `Raster*`, [`SpatRaster`], or file paths to one or more raster
#'   files.
#' @param xlim Numeric vector. The longitudinal extent of the area to plot, in
#'   WGS84 coordinates.
#' @param ylim Numeric vector. The latitudinal extent of the area to plot, in
#'   WGS84 coordinates.
#' @param subset_layers Vector. A vector of layer names or numeric positions to
#'   be included in plot. Only relevant if `ras` has multiple layers/bands. If
#'   missing all layers are included.
#' @param layer_names Character vector. A vector of panel titles (only relevant
#'   if ras is a stack). If not provided, names from ras will be used. If
#'   provided, length must equal `terra::nlyr(ras)`, or match
#'   `length(subset_layers)`.
#' @param legend_title Character. Legend title.
#' @param set_value_range A numeric vector containing an upper and lower bound
#'   value (in units of raster). Values outside this range (including values
#'   falling on the boundaries) will be masked.
#' @param scale_type Character. One of:
#'   - `"none"` (raw data, no rescaling);
#'   - `"log10"`;
#'   - `"max normalize"` (proportional to maximum value);
#'   - `"minmax normalize"` (rescale values to be between zero and 1 based
#'   on min and max); or
#'   - `"discrete"`.
#'   Note that if `"log10"` is used 0 or 1 values. must be masked
#'   (using `set_value_range`) or rescaled outside of this function.
#' @param basemap_mode Either `'boundaries'` or `'osm'` (i.e., OpenStreetMap),
#'   defining whether OpenStreetMap imagery should be used for static map
#'   basemaps, or simple administrative boundaries. Default is `'osm'`.
#' @param transparency Numeric. Transparency of raster, between 0-1.
#' @param colramp_entire_range Logical. Whether to set colour ramp limits based
#'   on national risk range (`TRUE`) or by risk range present in region
#'   specified by `xlim` and `ylim`.
#' @param surveillance_locs A spatial object or a path to a .csv file containing
#'   columns named "Latitude" and "Longitude".
#' @param shape shape(s) to use for surveillance locations. Default is 21. If
#'   you have multiple bands, and you'd like to use different symbols on each
#'   panel, specify a vector of shapes. (e.g. c(NA, 21,21,21) will not include
#'   shapes on the first panel, but will on the rest
#' @param pt_col Character. The plotting colour for surveillance points.
#' @param aggregate_raster `NULL` or a list containing the aggregation factor
#'   (i.e. number of raster cells to aggregate) and the aggregation function
#'   e.g. `list(10, sum)`.
#' @param nrow For multipanel plots, an optional  numeric value defining the
#'   number of rows of panels.
#' @param height height of plot in inches (will be rendered at 300 dpi). If not
#'   defined will use size of current graphic device. Width will be determined
#'   automatically, based on the aspect ratio given by the plotting extent.
#' @param outfile Character. File path to save map.
#' @return If `outfile` is provided, a map is saved to that file. Otherwise, a
#'   `tmap` object is returned to R.
#' @details This function relies on the `OpenStreetMap` package to obtain base
#'   layer tiles. This in turn requires Java to be installed, and linking R with
#'   Java can sometimes prove challenging. On macOS, Java errors can often be
#'   resolved by entering `sudo R CMD javareconf` in a terminal ( which updates
#'   R's Java configuration variables). On Windows, ensure that the Java
#'   architecture (32-bit/64-bit) matches that of R. Additionally, some Java
#'   errors arise when using RStudio but not when using R.
#' @importFrom dplyr filter
#' @importFrom gdalUtilities gdalwarp
#' @importFrom terra aggregate minmax ncell setMinMax writeRaster nlyr rast ext
#' @importFrom sf st_as_sf st_crop st_crs st_read st_transform
#' @importFrom stats qlogis
#' @importFrom tmap tm_compass tm_dots tm_facets tm_layout tm_polygons tm_raster tm_rgb tm_scale_bar tm_shape tmap_mode tmap_options tmap_save
#' @importFrom tmaptools bb read_osm
#' @importFrom utils read.csv
#' @importFrom methods is
#' @importFrom magrittr %>%
#' @export
static_map <- function(ras, xlim, ylim, subset_layers, layer_names, legend_title,
                       set_value_range, scale_type = "none",
                       basemap_mode=c('osm', 'boundaries'),
                       transparency = 0.7, colramp_entire_range = TRUE,
                       surveillance_locs, shape = 21, pt_col = "red", aggregate_raster,
                       nrow, height, outfile) {

  if(missing(height)) {
    stop('height must be specified.')
  }

  # If legend is not specified make it blank
  if(missing(legend_title)) legend_title <- ''

  basemap_mode <- match.arg(basemap_mode)


  # If an incorrect scale_type is specified
  scale_type <- match.arg(scale_type, c(
    "none", "log10", "max normalize", "minmax normalize",
    "discrete"))

  # Load raster if file path is provided
  if(is.character(ras) || is(ras, 'Raster')) {
    ras <- terra::rast(ras)
  } else if(!is(ras, 'SpatRaster')) {
    stop('ras must be a Raster*, SpatRaster, or a character vector of ',
         'one or more paths to raster files.')
  }

  # Subset layers if subset_layeyrs is specified
  if(!missing(subset_layers)) {
    ras <- ras[[subset_layers]]
  }

  # If raster layer_names are not provided use defaults
  # If provided ensure length matches
  if(missing(layer_names)) {
    layer_names <- names(ras)
  } else if(length(layer_names) != terra::nlyr(ras)) {
    stop('layer_names has invalid length')
  }

  # Calculate min and max of raster layers for possible transformation
  terra::setMinMax(ras)

  # Set bounded box based on xlim and ylim
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
    ras <- terra::aggregate(ras, fact = aggregate_raster[[1]],
                            fun = aggregate_raster[[2]])
    ras[is.nan(ras)] <- NA
  }

  # Restrict Raster value range?
  if(!missing(set_value_range)) {
    ras[ras <= min(set_value_range) | ras >= max(set_value_range)] <- NA
  }

  minval <- min(terra::minmax(ras)) # min value after clamping values

  # If raster is not empty
  if(!is.na(minval)) {
    # Convert to log10 scale
    if(scale_type == "log10") {
      if(minval <= 0)  {
        stop('Cannot log transform raster containing zero or negative values.')
      } else {
        ras <- log10(ras)
      }
    }

    if(isTRUE(colramp_entire_range)) {
      # Max normalize
      if(scale_type == "max normalize") {
        ras <- max_normalize(ras)
      }

      # Min Max normalize
      if(scale_type == "minmax normalize") {
        ras <- min_max_normalize(ras)
      }
    }
  }

  # get min and max values again after transforming
  full_range <- range(terra::minmax(ras))

  # Reproject raster to web mercator
  # https://github.com/mtennekes/tmap/issues/410
  # https://github.com/mtennekes/tmap/issues/412
  terra::writeRaster(ras, f <- tempfile(fileext='.tif'))
  gdalUtilities::gdalwarp(
    f, f2 <- tempfile(fileext='.tif'),
    t_srs = "EPSG:3857",
    tr = if(basemap_mode=='osm') basemap_res else c(5000, 5000),
    r = if(scale_type=='discrete') "near" else "bilinear",
    te=e, te_srs='EPSG:4283')
  # ^ This will interpolate the aggregated data if
  #   aggregate_raster is not NULL
  ras <- terra::rast(f2)
  terra::setMinMax(ras)
  minval <- min(terra::minmax(ras))
  names(ras) <- layer_names

  if(is.na(minval)) warning('Raster has no data within extent and constrained range of values.')
  if(isTRUE(colramp_entire_range)) {
    limit_cols <- full_range
  } else {
    # Normalize over constrained range?
    # Max normalize
    if(scale_type == "max normalize") {
      ras <- max_normalize(ras)
    }

    # Min Max normalize
    if(scale_type == "minmax normalize") {
      ras <- min_max_normalize(ras)
    }
    # get min and max again after cropping/projecting
    minval <- min(terra::minmax(ras))
    maxval <- max(terra::minmax(ras))
    limit_cols <- c(minval, maxval)
  }


  # Set tmap options
  if(basemap_mode=='osm') {
    opts <- tmap::tmap_options(
      max.raster=c(plot=max(terra::ncell(basemap), terra::ncell(ras)),
                   view=max(terra::ncell(basemap), terra::ncell(ras))),
      overlays = c(Labels = paste0(
        "http://services.arcgisonline.com/arcgis/rest/services/Canvas/",
        "World_Light_Gray_Reference/MapServer/tile/{z}/{y}/{x}"))
    )
  } else {
    opts <- tmap::tmap_options(
      max.raster=c(plot=terra::ncell(ras), view=terra::ncell(ras)),
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


  m <- if(basemap_mode=='osm') {
    tmap::tm_shape(basemap) +
      tmap::tm_rgb()
  } else {
    tmap::tm_shape(basemap) +
      tmap::tm_polygons(col='white')
  }

  # If raster contains only NULLs don't set palette. Otherwise use inferno
  # Useful when plotting panels with empty rasters (due to probabilities below threshold etc)
  m <- m +
    tmap::tm_shape(ras) +
    tmap::tm_raster(palette= if(all(is.na(minval))) NULL else "inferno",
                    style='cont',
                    midpoint=NA,
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

  # Add panel labels if layers >1
  if(isTRUE(terra::nlyr(ras) >1)) {
    m <- m + tmap::tm_layout(panel.labels=layer_names)
    m <- m + tmap::tm_layout(legend.outside.position='bottom')
  }

  # If nrow is defined (otherwise default features used)
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
        sf::st_crop(y = terra::ext(ras)))
    )
    # Add surveillance locations to map
    m <- m +
      tmap::tm_shape(locs) +
      tmap::tm_dots(col = pt_col, shape=shape)
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
