#' Create a mask raster
#'
#' Create a mask raster, retaining NA and setting all non-NA cells to 1. Input
#' raster will be resampled if necessary, using "max" resampling (i.e. if any
#' contributing cells are not NA, the new cell will be set to 1).
#'
#' @param infile Character. File path to input raster file.
#' @param outfile Character. Output raster file path. Directory will be
#'   created (recursively) if it doesn't exist.
#' @param res Numeric. A vector of one or two numbers giving the desired output
#'   resolution. If missing, the input resolution will be used. If a single
#'   number is provided, it will be used for both horizontal and vertical
#'   resolution.
#' @param extent One of of: a numeric vector giving the desired extent of the
#'   output raster as `c(xmin, xmax, ymin, ymax)`; an [Extent][raster::extent()]
#'   object; or an object from which such an `Extent` object can be extracted.
#' @return A raster is written to `outfile`, with NA cells transferred
#'   from `infile` and all other cells set to 1.
#' @importFrom raster raster mask extent
#' @importFrom gdalUtilities gdalwarp
#' @importFrom sf st_bbox
#' @importFrom methods is
#' @export
na_mask <- function(infile, outfile, res, extent) {

  if(!missing(extent)) {
    if(is(extent, 'Extent')) {
      extent <- sf::st_bbox(extent)
    } else {
      tryCatch({
        extent <- raster::extent(extent)
      }, error=function(e) {
        stop('Cannot extract/construct extent from `extent`')
      })
    }
  }

  if(!missing(res)) {
    if(length(res)==1) {
      res <- c(res, res)
    } else if(length(res) > 2) {
      stop('If provided, res must be a numeric vector with 1 or 2 elements.')
    }
  }

  if(!dir.exists(dirname(outfile))) dir.create(dirname(outfile), recursive=TRUE)

  args <- list(srcfile=path.expand(infile),
               dstfile=f <- tempfile(fileext='.tif'),
               tr=if(!missing(res)) res else NULL,
               te=if(!missing(extent)) extent else NULL,
               et=0.01, ot='Int16', r='max')
  do.call(gdalUtilities::gdalwarp, args[!sapply(args, is.null)])
  r <- raster::raster(f)
  raster::mask(!is.na(r), r, filename=outfile, overwrite=TRUE)
  return(invisible(NULL))
}
