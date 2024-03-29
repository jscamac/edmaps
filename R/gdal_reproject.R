#' Reproject and resample a raster
#'
#' This function can be used to change resolution, projection and extent of a
#' raster.
#'
#' @param infile Character. File path to a raster file.
#' @param outfile Character. Output raster file path.
#' @param src_proj Character. The source file coordinate system. Only needs to
#'   be set if you wish to reproject object and it is not specified in the
#'   `infile`. The coordinate systems that can be passed includes EPSG
#'   PCS and GCSes (i.e. EPSG:4296), PROJ.4 declarations (as above), or the
#'   name of a .prj file containing well known text. Starting with GDAL 2.2,
#'   if the SRS has an explicit vertical datum that points to a PROJ.4
#'   geoidgrids, and the input dataset is a single band dataset, a vertical
#'   correction will be applied to the values of the dataset.
#' @param tgt_proj Character. The target coordinate system. See
#'   `src_proj` for details. If not specified, `outfile` will
#'   inherit the coordinate system of `infile`.
#' @param res Numeric vector containing x and y resolution. e.g.
#'   `c(xres, yres)`. Must be specified in the units of
#'   `tgt_proj`.
#' @param resampling_method Character. One of `"near"`,
#'   `"bilinear"`, `"cubic"`, `"cubicspline"`,
#'   `"lanczos"`, `"average"`, `"mode"`, `"max"`,
#'   `"min"`, `"med"`, `"q1"`, `"q3"`. See Details.
#'   Default is `"near"`.
#' @param tgt_extent Numeric vector containing corner coordinates specified as
#'   `c(xmin, ymin, xmax, ymax)`. Must be specified in the units of
#'   `tgt_proj`.
#' @param buffer Numeric. Add buffer (specified in units of `tgt_proj`).
#'   Useful for adding buffers around coasts, and for filling in small gaps
#'   within raster specified by `infile`.
#' @param src_nodata Integer. The nodata value for an input file.
#' @param datatype A character string indicating the output data type. See the
#'   [gdalwarp docs](https://gdal.org/programs/gdalwarp.html)
#'   for more information.
#' @param return_rast Logical. Return `RaserLayer` to R?
#' @param overwrite Logical. Should `outfile` be overwritten if it already
#'   exists?
#' @return_rast A raster file is produced on disk. Additionally, if
#'   `return_rast` is `TRUE` a `RasterLayer` object is returned
#'   to R.
#' @details The resampling methods available are as follows:
#' \itemize{
#' \item{near: nearest neighbour resampling (default, fastest algorithm, worst
#' interpolation quality).}
#' \item{bilinear: bilinear resampling.}
#' \item{cubic: cubic resampling.}
#' \item{cubicspline: cubic spline resampling.}
#' \item{lanczos: Lanczos windowed sinc resampling.}
#' \item{average: average resampling, computes the average of all non-NODATA
#' contributing pixels. (GDAL >= 1.10.0).}
#' \item{mode: mode resampling, selects the value which appears most often of
#' all the sampled points. (GDAL >= 1.10.0).}
#' \item{max: maximum resampling, selects the maximum value from all non-NODATA
#' contributing pixels. (GDAL >= 2.0.0).}
#' \item{min: minimum resampling, selects the minimum value from all non-NODATA
#' contributing pixels. (GDAL >= 2.0.0).}
#' \item{med: median resampling, selects the median value of all non-NODATA
#' contributing pixels. (GDAL >= 2.0.0).}
#' \item{q1: first quartile resampling, selects the first quartile value of all
#' non-NODATA contributing pixels. (GDAL >= 2.0.0).}
#' \item{q3: third quartile resampling, selects the third quartile value of all
#' non-NODATA contributing pixels. (GDAL >= 2.0.0).}
#' }
#' @importFrom raster raster res
#' @importFrom gdalUtilities gdalwarp
#' @export
gdal_reproject <- function(infile, outfile, src_proj, tgt_proj, res,
  resampling_method = "near", tgt_extent, buffer, src_nodata,
  datatype='Float32',return_rast=FALSE, overwrite=TRUE) {

  resampling_method <- match.arg(
    resampling_method,
    c("near", "bilinear", "cubic", "cubicspline", "lanczos", "average",
      "mode", "max", "min", "med", "q1", "q3")
  )

  # permit raster package dataTypes, but standardise to GDAL nomenclature
  datatype <- match.arg(datatype, c(
    'Byte', 'Int16', 'UInt16', 'Int32', 'Float32', 'Float64',
    'INT1U', 'INT2S', 'INT2U', 'INT4S', 'FLT4S', 'FLT8S'))
  dt_raster <- c(INT1U='Byte', INT2S='Int16', INT2U='UInt16', INT4S='Int32',
                 FLT4S='Float32', FLT8S='Float64')
  if(datatype %in% names(dt_raster)) {
    datatype <- unname(dt_raster[datatype])
  }

  # if scalar passed to res, replicate to length 2
  if(!missing(res) && length(res)==1) res <- c(res, res)
  if(missing(res)) res <- raster::res(raster::raster(infile))

  # ensure buffer is numeric if provided
  if(!missing(buffer) && !is.numeric(buffer))
    stop('If provided, buffer must be numeric')
  # ensure buffer is not less than output res
  if(!missing(buffer) && buffer < res[1])
    stop('If provided, buffer must be numeric')

  # get standard gdal arg names
  name_match <- c(
    infile='srcfile', outfile='dstfile', src_proj='s_srs',
    tgt_proj='t_srs', res='tr', resampling_method='r', tgt_extent='te',
    src_nodata='srcnodata', datatype='ot')
  args <- mget(names(formals()), sys.frame(sys.nframe()))
  args <- args[!sapply(args, is.symbol)]
  names(args) <- name_match[names(args)]
  args <- args[!is.na(names(args))]

  # Create directory if it does not exist
  if(!dir.exists(dirname(outfile))) {
    dir.create(dirname(outfile), recursive = TRUE)
  }

  message('Resampling raster')
  do.call(gdalUtilities::gdalwarp, c(args, overwrite=overwrite))

  if(!missing(buffer)) {
    if(buffer > 0) {
      r <- raster::raster(outfile)
      w <- raster::focalWeight(r, buffer, type='circle')
      fill_na(r, median, w, outfile, overwrite=TRUE)
    }
  }

  if(isTRUE(return_rast)) raster::raster(outfile) else invisible(NULL)

}
