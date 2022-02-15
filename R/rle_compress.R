#' Compress raster data using run length encoding
#'
#' Compress categorical raster data using run length encoding.
#'
#' @param x File path to the categorical raster to be compressed, or a
#'   `Raster*` object.
#' @param outfile Character (optional). Path to target .rds file that will
#'   store RLE results. Directory will be created recursively if it doesn't
#'   exist.
#' @param quiet Logical. Should progress messages be suppressed?
#' @return A list with five elements:
#'   * `starts`: Cell numbers corresponding to run starts
#'   * `lengths`: Run lengths
#'   * `values`: Run values
#'   * `extent`: Raster extent
#'   * `res`: Raster resolution
#'
#' This object is additionally saved in rds format to `outfile`, if
#' provided.
#' @importFrom raster raster blockSize getValues extent res
#' @importFrom furrr future_map future_options
#' @export
rle_compress <- function(x, outfile, quiet=FALSE) {
  now <- Sys.time()
  if(is.character(x)) x <- raster::raster(x)
  if(!missing(outfile) && !dir.exists(dirname(outfile))) {
    dir.create(dirname(outfile), recursive=TRUE)
  }
  bs <- raster::blockSize(x)
  if(!isTRUE(quiet)) message(sprintf('Processing raster (%s blocks)', bs$n))
  outdir_len <- sprintf('%s/%s_len', tempdir(), round(as.numeric(now)))
  outdir_val <- sprintf('%s/%s_val', tempdir(), round(as.numeric(now)))
  if(!dir.exists(outdir_len)) dir.create(outdir_len)
  if(!dir.exists(outdir_val)) dir.create(outdir_val)
  timespent <- Sys.time() - now
  if(!isTRUE(quiet)) {
    message(sprintf('%.02f %s: Writing intermediate files to:\n  - %s\n  - %s',
                    timespent, attr(timespent, 'unit'),
                    outdir_len, outdir_val))
  }
  furrr::future_map(1:bs$n, function(i) {
    v <- raster::getValues(x, row=bs$row[i], nrows=bs$nrows[i])
    v[is.na(v)] <- Inf
    out <- rle(v)
    cat(out$lengths, file=file.path(
      outdir_len,
      sprintf(sprintf('len_%%0%sd.txt', nchar(bs$n)), i)))
    cat(out$values, file=file.path(
      outdir_val,
      sprintf(sprintf('val_%%0%sd.txt', nchar(bs$n)), i)))
  }, .options =
    furrr::furrr_options(globals=c('bs', 'x', 'outdir_len', 'outdir_val'))
  )

  timespent <- Sys.time() - now
  if(!isTRUE(quiet)) {
    message(sprintf('%.02f %s: Combining RLE vectors from chunked output',
                    timespent, attr(timespent, 'unit')))
  }
  len <- unlist(lapply(list.files(outdir_len, '^len_\\d+\\.txt$',
                                  full.names=TRUE), scan, quiet=TRUE))
  val <- unlist(lapply(list.files(outdir_val, '^val_\\d+\\.txt$',
                                  full.names=TRUE), scan, quiet=TRUE))
  val[is.infinite(val)] <- NA
  starts <- c(1, cumsum(len)[-length(len)] + 1)
  out <- list(starts=starts, lengths=len, values=val,
              extent=raster::extent(x), res=raster::res(x),
              crs=raster::crs(x))
  class(out) <- 'raster_rle'
  if(!missing(outfile)) saveRDS(out, outfile)
  out
}
