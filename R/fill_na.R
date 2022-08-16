#' Modify NAs within neighbourhood of data cells
#'
#' Apply a function to NA cells within a circular moving window.
#'
#' @param x A `RasterLayer` or single-layer [`SpatRaster`] object, or a path to
#'   a raster file.
#' @param fun The function (name or symbol) to apply to the moving windows.
#'   First argument should represent the vector of cells contained in the focal
#'   window. Only non-NA cell values will be passed to the function.
#' @param dist The distance, in units of the CRS of `x`, used as the radius of a
#'   circle defining the neighbourhood of an NA cell. The median of non-NA cells
#'   within the neighbourhood is assigned to the NA cell. Cells within the
#'   neighbourhood contribute equally to the median.
#' @param outfile Character. File path to an output raster file. If missing, a
#'   temporary file will be used.
#' @param return_rast Logical. Should the resulting [`SpatRaster`] be returned
#'   to R?
#' @param overwrite Logical. Should `outfile` be replaced if it already
#'   exists?
#' @details NA values within focal blocks will be ignored. Raster edges will be
#'   NA-padded to allow focal computations at the edge (see [terra::focal()]).
#' @return Returns the resulting [`SpatRaster`] if `return_rast` is `TRUE`.
#'   Returns the output file path otherwise.
#' @importFrom terra focal focalMat rast xres yres
#' @export
fill_na <- function(x, fun, dist, outfile, return_rast=FALSE, overwrite=FALSE) {
  if(missing(outfile)) outfile <- tempfile(fileext='.tif')
  if(is.character(x) || is(x, 'RasterLayer')) {
    x <- terra::rast(x)
  } else if(!is(x, 'SpatRaster') || dim(x)[3] > 1) {
    stop('x must be a RasterLayer object, single-layer SpatRaster object, ',
         'or a path to a raster file.', call.=FALSE)
  }
  if(terra::xres(x) != terra::yres(x))
    stop('x must have equal horizontal and vertical resolution.')
  fx <- match.fun(fun)
  if(missing(outfile)) outfile <- tempfile(fileext='.tif')

  m <- terra::focalMat(x, dist, type='circle', fillNA=TRUE)
  out <- terra::focal(x, w=m/m, function(z, ...) fx(z, na.rm=TRUE),
                      na.policy='only', filename=outfile, overwrite=overwrite)

  if(isTRUE(return_rast)) out else invisible(outfile)
}
