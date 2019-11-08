#' Download climate layers from WorldClim 2.0
#'
#' Download climate layers from WorldClim 2.0 (current climate only).

#' @param outfile Character. Target file for downloaded .zip archive.
#' @param variable Character. Can be one of \code{"bio"} (19 standard bioclim
#'   variables), \code{"tmin"}, \code{"tmax"}, \code{"tavg"}, \code{"srad"},
#'   \code{"wind"} or \code{"vapr"}.
#' @param resolution Character. Can be one of \code{"10m"}, \code{"5m"},
#'   \code{"2.5m"}, or \code{"30s"}.
#' @return A zipfile is downloaded and \code{NULL} is returned invisibly.
#' @seealso \code{\link{extract_worldclim2}}
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
  base_url <- "http://biogeo.ucdavis.edu/data/worldclim/v2.0/tif/base/wc2.0"
  url <- sprintf('%s_%s_%s.zip', base_url, resolution, variable)

  # Download ZIP file
  download.file(url, outfile, mode='wb')
}
