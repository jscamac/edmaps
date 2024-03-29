#' Modify NAs within neighbourhood of data cells
#'
#' Apply a function to NA cells within a moving window.
#'
#' @param x A `Raster*` object.
#' @param fun The function (name or symbol) to apply to the moving windows.
#'   First argument should represent the vector of cells contained in the focal
#'   window. Only non-NA cell values will be passed to the function.
#' @param w A weights matrix (see [raster::focalWeight()] and [raster::focal()]
#'   defining the focal window to which fun will be applied. Note that all
#'   contributing cells will be given equal weight (i.e. varying weights are not
#'   respected - non-zero & non-NA weights will be replaced by 1).
#' @param outfile Character. File path to an output raster file. If missing, a
#'   temporary file will be used.
#' @param return_rast Logical. Should the `RasterLayer` be returned to R?
#' @param overwrite Logical. Should `outfile` be replaced if it already
#'   exists?
#' @details NA values within focal blocks will be ignored. Raster edges will be
#'   NA-padded to allow focal computations at the edge (see [raster::focal()].
#' @return Returns the resulting `RasterLayer` if `return_rast` is
#'   `TRUE`. Returns the output file path otherwise.
#' @importFrom raster raster xres yres boundaries focal res
#' @importFrom sf st_union st_buffer write_sf st_bbox st_as_sf
#' @importFrom stars st_as_stars
#' @importFrom gdalUtilities gdal_rasterize
#' @export
fill_na <- function(x, fun, w, outfile, return_rast=FALSE, overwrite=FALSE) {
  if(missing(outfile)) outfile <- tempfile(fileext='.tif')
  if(is.character(x)) x <- raster::raster(x)
  if(raster::xres(x) != raster::yres(x))
    stop('x must have equal horizontal and vertical resolution.')
  fx <- match.fun(fun)
  edges <- raster::boundaries(x, type='inner', asNA=TRUE)
  p <- sf::st_as_sf(stars::st_as_stars(edges), merge=TRUE)
  maxdist <- floor(nrow(w)/2)*raster::xres(x)
  b <- sf::st_union(sf::st_buffer(p, maxdist))
  sf::write_sf(b, f <- tempfile(fileext='.gpkg'))
  gdalUtilities::gdal_rasterize(
    f, f2 <- tempfile(fileext='.tif'),
    tr=raster::res(x), te=sf::st_bbox(x),
    burn=1, init=0, a_nodata=0, ot='Byte')
  br <- raster::raster(f2)
  x2 <- x
  x2[is.na(x) & is.na(br)] <- -Inf
  out <- raster::focal(
    x2, w=w/w, fun=function(x) fx(x[is.finite(x)]),
    pad=TRUE, NAonly=TRUE, filename=outfile, overwrite=overwrite)

  if(isTRUE(return_rast)) out else invisible(outfile)
}
