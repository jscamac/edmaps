#' Download climate layers from WorldClim 2.0
#'
#' Download climate layers from WorldClim 2.0 (current climate only).

#' @param outfile Character. Target file for downloaded .zip archive.
#' @param variable Character. Can be one of `"bio"` (19 standard bioclim
#'   variables), `"tmin"`, `"tmax"`, `"tavg"`, `"srad"`,
#'   `"wind"` or `"vapr"`.
#' @param resolution Character. Can be one of `"10m"`, `"5m"`,
#'   `"2.5m"`, or `"30s"`.
#' @return A zipfile is downloaded and `NULL` is returned invisibly.
#' @seealso [extract_worldclim2()]
#' @importFrom utils download.file
#' @export
#' @examples
#' \dontrun{
#' download_worldclim2('bioclim_10m', 'bio', '10m')
#' }
download_worldclim2 <- function(outfile, variable, resolution) {

  variable <- match.arg(
    variable, c("bio", "tmin", "tmax", "tavg", "srad", "wind", "vapr"))
  resolution <- match.arg(resolution, c("10m", "5m","2.5m","30s"))

  if(!dir.exists(dirname(outfile))) dir.create(dirname(outfile))

  # Construct URL
  base_url <- "https://biogeo.ucdavis.edu/data/worldclim/v2.1/base/wc2.1"
  url <- sprintf('%s_%s_%s.zip', base_url, resolution, variable)

  # Download ZIP file
  download.file(url, outfile, mode='wb')
}
