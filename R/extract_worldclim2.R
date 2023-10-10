#' Extract WorldClim 2.0 data
#'
#' Extract WorldClim 2.0 data
#'
#' @param file Character. Path to .zip archive downloaded by
#'   [download_worldclim2()].
#' @param outdir Character. File path to which contained files should be
#'   extracted. Will be created (recursively) if necessary.
#' @return Raster data are extracted to `outdir` and file paths of extracted
#'   files are returned invisibly.
#' @seealso [download_worldclim2()]
#' @importFrom utils unzip
#' @export
#' @examples
#' \dontrun{
#' download_worldclim2('bioclim_10m.zip', 'bio', '10m')
#' extract_worldclim2('bioclim_10m.zip', outdir='bioclim')
#' }
extract_worldclim2 <- function(file, outdir) {

  # Unzip and return tif filenames
  x <- grep('\\.tif$', utils::unzip(file, exdir = outdir), value=TRUE)
  x_new <- sprintf(
    '%s/%s%02d.tif', dirname(x),
    sub('wc[0-9.]+_\\d+[ms]_([^_]+)_.*', '\\1', basename(x)),
    as.numeric(gsub('^.*_(\\d+)\\.tif$', '\\1', basename(x)))
  )

  # Rename files into something more managable
  file.rename(from=x, to=x_new)

  return(invisible(sort(x_new)))
}
