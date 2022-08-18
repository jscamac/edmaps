#' Rasterize spatial vector dataset
#'
#' Converts vector object to a raster with specified extent and resolution.
#'
#' @param vector_data Character. File path to vector spatial data.
#' @param outfile Raster output file path. Parent directory will be created
#'   recursively if required.
#' @param template_raster Optional. `Raster*`, [`SpatRaster`], or a file path to
#'   template raster. If this is provided, `extent`, `res`, and `crs` will be
#'   taken from this raster unless they are also passed to this function. If `x`
#'   is not provided, then `extent` and `res` must be provided.
#' @param extent Either a character path to a raster file, an
#'   [`SpatExtent`] object (or an object from which such an
#'   extent can be extracted), or a numeric vector with four elements giving
#'   xmin, xmax, ymin, ymax.
#' @param res Numeric or integer vector giving the horizontal and vertical
#'   spatial resolution of the target raster, in units of `crs`. If a single
#'   value is given, it will be used for both horizontal and vertical
#'   resolution.
#' @param crs Target coordinate reference system as a PROJ string (character) an
#'   object of class CRS. If missing and `template_raster` is supplied, the crs
#'   of `template_raster` will be used. If `template_raster` is also not
#'   supplied, the CRS of `vector_data` will be used.
#' @param field Character. Name of attribute to be burned into raster.
#' @param burn_value Numeric. A constant value to burn into raster.
#' @param datatype Character. Output data type (see
#'   [gdal_rasterize(https://gdal.org/programs/gdal_rasterize.html)
#'   documentation).
#' @param overwrite Logical. Should outfile be overwritten if it exists?
#' @param return_rast Logical. Return object to R?
#' @return A binarized raster is written to `outfile`, and returned to R as a
#'   [`SpatRaster`] if `return_rast` is `TRUE`.
#' @importFrom terra rast ext crs res
#' @importFrom gdalUtilities gdal_rasterize
#' @importFrom methods is
#' @export
rasterize_vector <- function(vector_data, outfile, template_raster, extent,
  res, crs, field,  burn_value, datatype = "Float32", overwrite = FALSE,
  return_rast = FALSE) {

  # permit raster package dataTypes, but standardise to GDAL nomenclature
  datatype <- match.arg(datatype, c(
    'Byte', 'Int16', 'UInt16', 'Int32', 'Float32', 'Float64',
    'INT1U', 'INT2S', 'INT2U', 'INT4S', 'FLT4S', 'FLT8S'))
  dt_raster <- c(INT1U='Byte', INT2S='Int16', INT2U='UInt16', INT4S='Int32',
                 FLT4S='Float32', FLT8S='Float64')
  if(datatype %in% names(dt_raster)) {
    datatype <- unname(dt_raster[datatype])
  }
  if(!missing(field) && !missing(burn_value))
    stop('Please set either field or burn_value, not both.')
  if(missing(field) && missing(burn_value))
    stop("Please set field or burn_value.")
  if(missing(outfile)) outfile <- tempfile(fileext = '.tif')

  if(!missing(template_raster)) {
    template <- terra::rast(template_raster)
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

  if(length(res) == 1) res <- c(res, res)

  # Create directory if it does not exist
  if(!dir.exists(dirname(outfile))) {
    dir.create(dirname(outfile), recursive = TRUE)
  }

  initialise_raster(outfile=outfile, extent=extent, res=res, crs=crs,
                    datatype=datatype, overwrite=overwrite)
  args <- list(src_datasource=vector_data, dst_filename=outfile,
               a=if(!missing(field)) field else NULL,
               burn=if(!missing(burn_value)) burn_value else NULL)
  do.call(gdalUtilities::gdal_rasterize, args[!sapply(args, is.null)])

  if(isTRUE(return_rast)) terra::rast(outfile) else invisible(NULL)
}
