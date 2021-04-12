#' Produce an interactive html map
#'
#' Produce an interactive html map.
#'
#' @param ras A \code{RasterLayer} or file path to a raster file.
#' @param layer_name Character. An optional name to assign to \code{ras}.
#' @param palette Either a vector of 2 or more colours (e.g. as hex codes or
#'   colour names) or the name of a palette function supported by \code{tmap}
#'   (see \code{\link[tmaptools]{palette_explorer}} and
#'   \code{\link[tmap]{tm_raster}}).
#' @param transparency Numeric. Value between 0 and 1 defining the opacity of
#'   the plotted raster data (1 = fully opaque; 0 = fully transparent).
#' @param legend Logical. Should a legend be plotted?
#' @param set_value_range A numeric vector giving upper and lower limits for
#'   raster values. Values outside this range (including the limits) will be
#'   set to NA.
#' @param discrete Logical. Are the values of \code{ras} discrete
#'   (categorical)?
#' @param scale_type Character. Can be one of: \code{"none"} (raw data, no
#'   rescaling), \code{"log10"}, \code{"max normalize"} (proportional to
#'   maximum value), \code{"minmax normalize"} (rescale values to be between 0
#'   and 1 based on min and max) or \code{"logit"}. Note that if
#'   \code{"log10"} or \code{"logit"} is used, 0 or 1 values must be masked (
#'   using \code{set_value_range}) or rescaled before passing to this
#'   function. \code{scale_type} is ignored if \code{discrete} is \code{TRUE}.
#' @param outfile Character. If \code{NULL}, map will be returned to R and not
#'   saved. Otherwise, map will be exported as a html file. Full path address
#'   must be used. If pandoc is available, a standalone html file is created
#'   (see details).
#' @param surveillance_locs A spatial object or a path to a .csv file
#'   containing columns named "Latitude" and "Longitude".
#' @param pt_col Character. The plotting colour for surveillance points.
#' @param cleanup Logical. If a standalone html file is created, should
#'   accessory files be removed after the standalone file is generated? This
#'   will be a folder created within \code{tempdir()}.
#' @return A html map.
#' @details To create a standalone html file, the
#'   \href{https://pandoc.org/installing.html}{pandoc} software must be
#'   installed and available to R. If pandoc is unavailable, the html file
#'   will be accompanied by a folder of accessory files.
#' @importFrom raster extent getValues maxValue minValue ncell projection projectRaster raster setMinMax setValues writeRaster
#' @importFrom gdalUtilities gdalwarp
#' @importFrom methods is slot "slot<-"
#' @importFrom utils read.csv
#' @importFrom dplyr filter
#' @importFrom sf st_as_sf st_crs st_crop st_transform
#' @importFrom stats qlogis
#' @importFrom tmap tm_dots tm_facets tm_raster tm_scale_bar tm_shape tmap_leaflet tmap_mode tmap_options
#' @importFrom tmaptools tmap.pal.info
#' @importFrom leafem addMouseCoordinates
#' @importFrom htmlwidgets saveWidget
#' @importFrom leaflet addMiniMap
#' @importFrom stars read_stars
#' @export
interactive_map <- function(ras, layer_name = NULL, palette = 'inferno',
                            transparency = 0.8, legend = TRUE, set_value_range = NULL,  discrete = FALSE,
                            scale_type = "none", outfile = NULL, surveillance_locs = NULL,
                            pt_col = "red", cleanup = FALSE) {

  # Ensure valid scaling type
  scale_type <- match.arg(
    scale_type,
    c("none", "log10", "max normalize", "minmax normalize", "logit", "percent"))

  if(!is.null(outfile)) {
    if(!grepl('\\.html$', outfile))
      stop('outfile should have a .html extension.')
    # Create directory if it does not exist
    if(!dir.exists(dirname(outfile))) {
      dir.create(dirname(outfile), recursive = TRUE)
    }
  }

  # Must be a path to a raster file, or a RasterLayer
  if(is.character(ras)) ras <- raster::raster(ras)
  ras <- raster::setMinMax(ras)


  # If projection is missing, assume WGS84
  if(is.na(raster::projection(ras))) {
    warning('ras assumed to have WGS84 coordinate system (EPSG:4326)')
    raster::projection(ras) <- '+init=epsg:4326'
  }

  # if palette has length = 1, it must be a supported palette name.
  if(length(palette) == 1) palette <-
    match.arg(palette, rownames(tmaptools::tmap.pal.info))


  # Restrict Raster value range?
  if(!is.null(set_value_range)) {
    ras[ras <= min(set_value_range) | ras >= max(set_value_range)] <- NA
  }

  minval <- raster::minValue(ras)

  if(!is.na(minval)) {

    maxval <- raster::maxValue(ras)

    # if the raster contains non-zero values...
    if(!isTRUE(discrete)) {
      # Convert to log10 scale
      if(scale_type == "log10") {
        if(minval <= 0) {
          stop('Cannot log transform raster containing zero or negative values.')
        } else {
          ras <- raster::setValues(ras, log10(raster::getValues(ras)))
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
        if(minval <= 0 | maxval >= 1) {
          stop('Cannot logit transform raster containing values less than or',
               ' equal to 0 or values greater than or equal to 1.')
        } else {
          ras <- raster::setValues(ras, stats::qlogis(raster::getValues(ras)))
        }
      }

      # Convert to percent scale
      if(scale_type == "percent") {
        if(minval < 0 | maxval > 1) {
          stop('Cannot convert to percentage raster because there are values',
               ' less than 0 or values greater 1.')
        } else {
          ras <- raster::setValues(ras, raster::getValues(ras)*100)
        }
      }
    }

  }

  # Flip legend
  flip_legend <- function(m) {
    # m: a `mapview` or `leaflet` object
    if(methods::is(m, 'mapview')) {
      calls <- methods::slot(m, 'map')$x$calls
    } else if((methods::is(m, 'leaflet'))) {
      calls <- m$x$calls
    } else {
      stop('m must be a mapview or leaflet object.')
    }
    i <- grep('addLegend', sapply(calls, '[[', 'method'))
    clr <- calls[[i]]$args[[1]]$colors
    # Only flip if there is more than one colour to be plotted. Check this by
    # searching for whitespace in the colour string.
    if(length(clr) == 1 && grepl(' ', clr)) {
      h <- strsplit(gsub('(#\\S+)[^#]+', '\\1 ', clr), ' ')[[1]]
      p <- paste0(rev(
        100 - as.numeric(strsplit(gsub('#\\S+[ ,]+|%', '', clr), ', ')[[1]])
      ), '%')
      clr2 <- paste0(do.call(paste, list(rev(h), c('', p, ''))), collapse=', ')
      calls[[i]]$args[[1]]$colors <- clr2
      calls[[i]]$args[[1]]$labels <- rev(calls[[i]]$args[[1]]$labels)
      extra <- calls[[i]]$args[[1]]$extra
      calls[[i]]$args[[1]]$extra$p_1 <- 1 - extra$p_n
      calls[[i]]$args[[1]]$extra$p_n <- 1 - extra$p_1
      if(methods::is(m, 'mapview')) {
        methods::slot(m, 'map')$x$calls <- calls
      } else if((methods::is(m, 'leaflet'))) {
        m$x$calls <- calls
      }
    }
    m
  }

  # Initialise interactive map
  opts <- tmap::tmap_options(
    max.raster=c(plot=raster::ncell(ras),
                 view=raster::ncell(ras)),
    basemaps = c(OpenStreetMap="OpenStreetMap", Canvas = "Esri.WorldGrayCanvas",
                 Topo="Esri.WorldTopoMap", Imagery = "Esri.WorldImagery"),
    overlays = c(Labels = paste0(
      "http://services.arcgisonline.com/arcgis/rest/services/Canvas/",
      "World_Light_Gray_Reference/MapServer/tile/{z}/{y}/{x}"))
  )
  suppressMessages(tmode <- tmap::tmap_mode('view'))
  on.exit({
    tmap::tmap_options(opts)
    suppressMessages(tmap::tmap_mode(tmode))
  })

  if(is.na(minval)) {
    ras <- raster::projectRaster(ras, crs = "+init=epsg:3857", method = "ngb")

  } else if(isTRUE(discrete)) {
    # If raster is entirely NA, tmap throws an error if palette is specified.
    # Can't just `return` early, since file_out needs to be fulfilled.
    # Workaround is to set a colour, but set transparency to a small number.
    ras <- raster::projectRaster(ras, crs = "+init=epsg:3857", method = "ngb")
    m <- tmap::tm_shape(ras, name=layer_name) +
      tmap::tm_raster(col='white', style='cat', title=layer_name,
                      alpha=0.01)
  } else {
    raster::writeRaster(ras, f <- tempfile(fileext='.tif'))
    gdalUtilities::gdalwarp(f, f2 <- tempfile(fileext='.tif'),
                            t_srs = "+init=epsg:3857", r = "bilinear")
    ras <- stars::read_stars(f2)
    m <- tmap::tm_shape(ras, name = layer_name) +
      tmap::tm_raster(palette = palette,
                      style = "cont", midpoint = NA, alpha = transparency,
                      legend.show=TRUE, title = layer_name,
                      colorNA='#ffffff01') +
      # ^ colorNA set to almost transparent, to avoid error if
      #   raster is entirely NA.
      tmap::tm_facets(as.layers=TRUE)
  }


  # Add trap locations (if required)
  if(!is.null(surveillance_locs)) {
    if(is.character(surveillance_locs)) {
      surveillance_locs <- utils::read.csv(surveillance_locs) %>%
        dplyr::filter(!is.na(Longitude),
                      !is.na(Latitude))
    }
    locs <- suppressWarnings(
      sf::st_as_sf(surveillance_locs, coords = c("Longitude","Latitude"),
                   crs = 4326) %>%
        sf::st_transform(crs = sf::st_crs(ras)) %>%
        sf::st_crop(y = raster::extent(ras))
    )

    pts <- tmap::tm_shape(locs) +
      tmap::tm_dots(col=pt_col, shape=21)

    m <- m + pts
  }

  # Convert tmap to leaflet, flip legend and add functionality
  l <- (m + tmap::tm_scale_bar()) %>%
    tmap::tmap_leaflet()

  out <- l %>%
    flip_legend %>%
    leaflet::addMiniMap(position='bottomleft', toggleDisplay=TRUE) %>%
    leafem::addMouseCoordinates()

  # outfile supplied
  if(!is.null(outfile)) {
    # If pandoc is installed, make self-contained html.
    if(system('pandoc -v')==0) {
      # Save to tempdir and then move selfcontained to target path.
      outfile_tmp <- file.path(tempdir(), basename(outfile))
      # (note path construction to overcome bug
      # https://github.com/ramnathv/htmlwidgets/issues/299)
      htmlwidgets::saveWidget(out, selfcontained=TRUE, file=outfile_tmp,
                              libdir=file.path(tempdir(), 'widget_files'))
      file.copy(outfile_tmp, outfile)
      if(isTRUE(cleanup)) {
        try(unlink(file.path(tempdir(), 'widget_files'), recursive=TRUE))
        try(unlink(outfile_tmp))
      }
    } else {
      htmlwidgets::saveWidget(out, selfcontained=FALSE,
                              file=file.path(normalizePath(dirname(outfile)),
                                             basename(outfile)))
    }
  } else {
    # Else return object in R
    return(out)
  }
}
