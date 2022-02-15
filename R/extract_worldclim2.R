#' Extract WorldClim 2.0 data
#'
#' Extract WorldClim 2.0 data
#'
#' @param path_2_zip Character. Path to .zip archive downloaded by
#'   [download_worldclim2()].
#' @param outdir Character. File path to which contained files should be
#'   extracted. Will be created (recursively) if necessary.
#' @return Raster data are extracted to `outdir` and `NULL` is
#'   returned invisibly.
#' @seealso [download_worldclim2()]
#' @importFrom utils unzip
#' @export
#' @examples
#' \dontrun{
#' download_worldclim2('bioclim_10m.zip', 'bio', '10m')
#' extract_worldclim2('bioclim_10m.zip', outdir='bioclim')
#' }
extract_worldclim2 <- function(path_2_zip, outdir) {

  # Unzip and return tif filenames
  x <- grep('\\.tif$', utils::unzip(path_2_zip, exdir = outdir),
            value=TRUE)

  # Rename files into something more managable
  file.rename(
    from=x,
    to=sprintf(
      fmt='%s/%s%02d.tif',
      dirname(x),
      gsub('wc2\\.0|\\d+[ms]|_|\\d+\\.tif', '', basename(x)),
      # ^ strip out all but variable name
      as.numeric(sub('.*_(\\d+)\\.tif', '\\1', basename(x))))
      # ^ 0-padded month/variable number
  )

  return(invisible(NULL))
}
