#' Create an empty raster
#'
#' Create an empty or constant raster with specified attributes.
#'
#' @param x Optional. Raster* or [`SpatRaster`] object or a file path to
#'   template raster. If this is provided, `extent`, `res`, and `crs` will be
#'   taken from this raster unless they are also passed to this function. If `x`
#'   is not provided, then `extent` and `res` must be provided.
#' @param outfile Target raster file path. Directory will be created
#'   (recursively) if it doesn't exist.
#' @param extent Either a character path to a raster file, an [`SpatExtent`]
#'   object (or an object from which such an extent can be extracted), or a
#'   numeric vector with four elements giving xmin, xmax, ymin, ymax.
#' @param res Numeric or integer vector giving the horizontal and vertical
#'   spatial resolution of the target raster, in units of `crs`. If a single
#'   value is given, it will be used for both horizontal and vertical
#'   resolution.
#' @param crs Target coordinate reference system as a PROJ string (character) or
#'   an object of class CRS. If missing and `x` is supplied, the crs of `x` will
#'   be used.
#' @param init Numeric. A value assigned to all cells of the created raster.
#' @param datatype Character. Data type for the created raster. See
#'   [terra::writeRaster()].
#' @param overwrite Logical. Should `outfile` be overwritten if it exists?
#' @param return_rast Logical. Should the resulting [`SpatRaster`] be returned?
#' @return An empty raster is created at `outfile`, and the corresponding
#'   [`SpatRaster`] is returned if `return_rast` is `TRUE`.
#' @importFrom terra rast writeRaster ext crs
#' @importFrom methods is
#' @export
initialise_raster <- function(x, outfile, extent, res, crs, init=NA,
  datatype='FLT4S', overwrite=TRUE,  return_rast=FALSE) {

  # permit gdal datatypes
  datatype <- match.arg(datatype, c(
    'Byte', 'Int16', 'UInt16', 'Int32', 'Float32', 'Float64',
    'LOG1S', 'INT1S', 'INT1U', 'INT2S', 'INT2U', 'INT4S', 'FLT4S', 'FLT8S'))
  dt_gdal <- c(Byte='INT1U', Int16='INT2S', UInt16='INT2U',
               Int32='INT4S', Float32='FLT4S', Float64='FLT8S')
  if(datatype %in% names(dt_gdal)) {
    datatype <- unname(dt_gdal[datatype])
  }

  if(!missing(x)) {
    template <- terra::rast(x)
    if(missing(extent)) extent <- terra::ext(template)
    if(missing(res)) res <- terra::res(template)
    if(missing(crs)) crs <- terra::crs(template)
  } else {
    if(missing(extent) || missing(res)) {
      stop('If x is not supplied, both extent and res must be supplied.')
    }
    if(missing(crs)) crs <- NA
    # extract extent from raster provided as file path
    if(is.character(extent)) extent <- terra::ext(terra::rast(extent))
    # extract extent from object
    if(!is(extent, 'SpatExtent')) extent <- terra::ext(extent)
  }
  if(!dir.exists(dirname(outfile))) dir.create(dirname(outfile), recursive=TRUE)
  r <- terra::rast(ext=extent, resolution=res, crs=crs, vals=init)
  terra::writeRaster(r, outfile, datatype=datatype, overwrite=overwrite,
                     gdal ='COMPRESS=LZW')

  if(isTRUE(return_rast)) r else invisible(NULL)
}
