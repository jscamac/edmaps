#' Compress raster data using run length encoding
#'
#' Compress categorical raster data using run length encoding.
#'
#' @param x Either a `RasterLayer` object, single-layer [`SpatRaster`] object,
#'   or file path to a categorical raster to be compressed.
#' @param outfile Character (optional). Path to target .rds file that will
#'   store run-length encoded results. Directory will be created recursively if
#'   it doesn't exist.
#' @param quiet Logical. Should progress messages be suppressed?
#' @param overwrite Logical. Should `outfile` be overwritten if it already
#'   exists? Ignored if `outfile` is not provided.
#' @return A `raster_rle` object (extending `tibble`) with three columns
#'   (`lengths`, `values`, and `starts`) and attributes (`extent`, a
#'   [`SpatExtent`] object, `res`, and `crs`). This object is additionally saved
#'   in rds format to `outfile`, if provided.
#' @importFrom terra rast blocks values ext res
#' @importFrom readr write_csv read_csv
#' @importFrom dplyr bind_rows mutate
#' @importFrom magrittr %>%
#' @importFrom methods is
#' @export
rle_compress <- function(x, outfile, quiet=FALSE, overwrite=FALSE) {
  if(!missing(outfile) && file.exists(outfile) && !overwrite) {
    stop(sprintf('outfile (%s) exists and overwrite=FALSE.', outfile),
         call.=FALSE)
  }
  now <- Sys.time()
  if(is.character(x) || is(x, 'RasterLayer')) x <- terra::rast(x)
  if(dim(x)[3] > 1) stop('x must have a single layer', call.=FALSE)
  if(!missing(outfile) && !dir.exists(dirname(outfile))) {
    dir.create(dirname(outfile), recursive=TRUE)
  }
  bs <- terra::blocks(x)
  if(!isTRUE(quiet)) message(sprintf('Processing raster (%s blocks)', bs$n))
  outdir <- sprintf('%s/%s', tempdir(), round(as.numeric(now)))
  if(!dir.exists(outdir)) dir.create(outdir)
  timespent <- Sys.time() - now
  if(!isTRUE(quiet)) {
    message(sprintf('%.02f %s: Writing intermediate files to %s',
                    timespent, attr(timespent, 'unit'),
                    outdir))
  }
  ff <- sapply(1:bs$n, function(i) {
    v <- terra::values(x, row=bs$row[i], nrows=bs$nrows[i])[, 1]
    v[is.na(v)] <- Inf
    out <- as.data.frame(unclass(rle(v)))
    f <- sprintf(sprintf('%s/%%0%sd.txt', outdir, nchar(bs$n)), i)
    readr::write_csv(out, f)
    f
  })

  timespent <- Sys.time() - now
  if(!isTRUE(quiet)) {
    message(sprintf('%.02f %s: Combining RLE vectors from chunked output',
                    timespent, attr(timespent, 'unit')))
  }
  runs <- lapply(ff, readr::read_csv, progress=!quiet, show_col_types=FALSE) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(values=ifelse(is.infinite(values), NA, values),
                  starts=c(1, cumsum(lengths)[-nrow(.)] + 1))

  attr(runs, 'extent') <- terra::ext(x)[1:4] # coerce to numeric vector to prevent passing pointer
  attr(runs, 'res') <- terra::res(x)
  attr(runs, 'crs') <- terra::crs(x)
  class(runs) <- c('raster_rle', class(runs))
  if(!missing(outfile)) saveRDS(runs, outfile)
  runs
}
