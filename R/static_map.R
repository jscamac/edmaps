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
#' @importFrom raster aggregate extent maxValue minValue ncell projectRaster stack setMinMax crop writeRaster
#' @importFrom sf st_as_sf st_crop st_crs
#' @importFrom stats qlogis
#' @importFrom tmap tm_dots tm_raster tm_rgb tm_shape tmap_mode tmap_options tmap_save tm_scale_bar tm_compass
#' @importFrom tmaptools bb read_osm
#' @importFrom utils read.csv
#' @importFrom magrittr "%>%"
#' @importFrom gdalUtilities gdalwarp
#' @export

static_map <- function(ras, xlim, ylim, layer,  legend_title, set_value_range, 
  scale_type = "none",  transparency = 0.7, colramp_entire_range = TRUE, 
  surveillance_locs, pt_col = "red", aggregate_raster, height, outfile) {
  
  if(missing(height)) {
    stop('height must be specified.')
  }
  
  # If an incorrect scale_type is specified
  scale_type <- match.arg(scale_type, c(
    "none", "log10", "max normalize", "minmax normalize", "logit", 
    "discrete"))

  if(is.character(ras)) ras <- raster::stack(ras)
  ras <- raster::setMinMax(ras)
  
  e <- tmaptools::bb(xlim=xlim, ylim=ylim)
  osm_rast <- tmaptools::read_osm(e, zoom=NULL)
  osm_rast_res <- c(abs(attr(osm_rast, 'dimensions')$x$delta), 
                    abs(attr(osm_rast, 'dimensions')$y$delta))
  
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
  
  # Scale raster values?
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
  
  full_range <- c(raster::minValue(ras), raster::maxValue(ras))

  # Reproject raster to web mercator
  # https://github.com/mtennekes/tmap/issues/410
  # https://github.com/mtennekes/tmap/issues/412
  if(scale_type == "discrete") {
    raster::projectRaster(
      ras, crs = "+init=epsg:3857", res = osm_rast_res,
      method = "ngb")
  } else {
    raster::writeRaster(ras, f <- tempfile(fileext='.tif'))
    gdalUtilities::gdalwarp(
      f, f2 <- tempfile(fileext='.tif'), 
      t_srs = "+init=epsg:3857", tr = osm_rast_res,
      r = "bilinear", te=sf::st_bbox(osm_rast))
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
  opts <- tmap::tmap_options(max.raster=c(
    plot=max(raster::ncell(osm_rast), raster::ncell(ras)), 
    view=max(raster::ncell(osm_rast), raster::ncell(ras))
  ),
  overlays = c(Labels = paste0(
    "http://services.arcgisonline.com/arcgis/rest/services/Canvas/",
    "World_Light_Gray_Reference/MapServer/tile/{z}/{y}/{x}"))
  )
  suppressMessages(tmode <- tmap::tmap_mode('plot'))
  on.exit({
    tmap::tmap_options(opts)
    suppressMessages(tmap::tmap_mode(tmode))
  })
  
  minval <- raster::minValue(ras)
  if(any(!is.na(minval))) {
    m <- tmap::tm_shape(osm_rast) +
      tmap::tm_rgb() +
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
  } else {
    m <- tmap::tm_shape(osm_rast) +
      tmap::tm_rgb() +
      tmap::tm_scale_bar(position=c('left', 'bottom'), text.size=0.75) +
      tmap::tm_compass() +
      tmap::tm_layout(outer.margins=c(0, 0, 0, 0),
                      legend.position=c('left', 'bottom'),
                      inner.margin=c(1/height, 0, 0, 0),
                      legend.text.size=0.8)
  }
  

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
