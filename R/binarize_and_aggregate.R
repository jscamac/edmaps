#' Binarize a categorical raster and optionally aggregate
#'
#' Binarize a categorical raster and optionally aggregate and/or resample.
#'
#' @param infile Character. File path to a categorical raster. If `rle` is
#'   provided, `infile` must not be provided.
#' @param rle A `raster_rle` object generated by [rle_compress()] or a file path
#'   to such an object saved as .rds.
#' @param outfile Character. The target file path for the binarized raster.
#' @param res output resolution as a numeric vector of 1 or 2 elements, or a
#'   `Raster*` object from which resolution can be extracted. If not
#'   provided, resolution will be taken from input. If `res` differs
#'   from that of `infile`, new cells will be assigned value 1 if any
#'   original cells belonging to `categories` have their centroids within
#'   the new cell.
#' @param extent Output extent as an `Extent` object or an object from
#'   which an Extent object can be extracted. If not provided, extent will be
#'   taken from input. If `extent` differs from that of `infile`,
#'   new cells will be assigned value 1 if any original cells belonging to
#'   `categories` have their centroids within the new cell.
#' @param categories Integer or numeric vector of class values to be laballed as
#'   1 in the target raster.
#' @param overwrite Logical. Should `outfile` be overwritten if it exists?
#' @param return_rast Logical. Should the target raster be returned to R as a
#'   `Raster` layer (`TRUE`) or not returned (`FALSE`)?
#' @param quiet Logical. Should progress messages be suppressed?
#' @return A binarized raster layer is written to `outfile`, and if
#'   `return_rast` is `TRUE`, the raster is additionally returned to R
#'   as a `Raster` layer.
#' @importFrom raster extension extent res raster xyFromCell cellFromXY writeRaster
#' @importFrom methods is
#' @export
binarize_and_aggregate <- function(infile, rle, outfile, extent, res, categories,
  overwrite=FALSE, return_rast=FALSE, quiet=FALSE) {
  now <- Sys.time()

  # ensure one of infile & rle is provided
  if(missing(infile) && missing(rle))
    stop('Provide infile or rle.')

  # ensure only one of infile & rle is provided
  if(!missing(infile) && !missing(rle))
    stop('Provide infile or rle, not both.')

  # if outfile is not provided, use a temp file
  if(missing(outfile)) outfile <- tempfile(fileext='.tif')

  # error if outfile exists and overwrite = FALSE
  if(file.exists(outfile) && !isTRUE(overwrite))
    stop('outfile exists: ', outfile)

  # Create directory if it does not exist
  if(!dir.exists(dirname(outfile))) {
    dir.create(dirname(outfile), recursive = TRUE)
  }

  # if rle is not provided, create raster_rle from infile
  if(missing(rle)) {
    rle <- rle_compress(infile, file.path(
      dirname(infile), raster::extension(basename(infile), 'rds')),
      quiet=quiet)
  } else if(is.character(rle)) {
    if(!isTRUE(quiet))
      message('Importing rle: ', rle)
    rle <- readRDS(rle)
  }

  # test that rle is now a raster_rle object
  if(!is(rle, 'raster_rle'))
    stop('rle should be a raster_rle object or a file path to such an object',
         ' saved as .rds')


  extent <- if(!missing(extent)) raster::extent(extent) else rle$extent
  if(missing(res)) res <- rle$res
  if(is.numeric(res)) {
    if(length(res) == 1) res <- c(res, res)
    if(length(res) > 2) stop('Supplied res invalid.')
  } else if(is(res, 'Raster')) {
    res <- raster::res(res)
  }

  # Process RLE data
  timespent <- Sys.time() - now
  if(!isTRUE(quiet)) {
    message(sprintf('%.02f %s: Extracting cell numbers for categories',
                    timespent, attr(timespent, 'unit')))
  }
  i <- which(rle$values %in% categories)
  r0 <- raster::raster(rle$extent, res=rle$res, crs=rle$crs)
  #r1 <- raster::raster(outfile)
  r1 <- raster::raster(extent, res=res, crs=rle$crs, vals=0)
  x2 <- cumsum(rle$lengths[i])
  timespent <- Sys.time() - now
  if(!isTRUE(quiet)) {
    message(sprintf('%.02f %s: Splitting cells into memory safe chunks',
                    timespent, attr(timespent, 'unit')))
  }
  x3 <- split(i, floor(x2 / 1e6)) # split into groups of max ~1 million cells
  # Below assumes `result` (a vector of all target cell numbers with 1 or more
  # (sub)cells belonging to `categories`) can fit in memory. Alternatively can
  # append cell1 (as xy) to fcsv at each pass through below loop.
  timespent <- Sys.time() - now
  if(!isTRUE(quiet)) {
    message(sprintf('%.02f %s: Reconstructing runs',
                    timespent, attr(timespent, 'unit')))
  }
  result <- unique(unlist(lapply(seq_along(x3), function(j) {
    if(interactive()) cat(sprintf('\r%.2f%%', j/length(x3)*100))
    z <- unlist(mapply(
      seq.int, rle$starts[x3[[j]]], length.out=rle$lengths[x3[[j]]],
      SIMPLIFY=FALSE))
    xy0 <- raster::xyFromCell(r0, z)
    cell1 <- as.integer(setdiff(raster::cellFromXY(r1, xy0), NA))
    cell1
  })))
  cat('\n')
  timespent <- Sys.time() - now
  if(!isTRUE(quiet)) {
    message(sprintf(
      paste0('%.02f %s: Filling raster output ',
             '(%s cells belong to categories)\nand writing to %s.',
             collapse=' '),
      timespent, attr(timespent, 'unit'), length(result),
      outfile)
    )
  }

  r1[result] <- 1
  r1[r1==0] <- NA
  # ^ initialising with 0 above and then reassigning NA avoids warnings about
  #   min/max Inf.
  raster::writeRaster(r1, outfile, datatype='INT2U', overwrite=overwrite)

  if(isTRUE(return_rast) || missing(outfile))
    raster::raster(outfile) else invisible(NULL)
}
