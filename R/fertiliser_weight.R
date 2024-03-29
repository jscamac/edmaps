#' Create a fertiliser weight raster
#'
#' Create a fertiliser weight raster as a function of estimated nrm fertiliser
#' tonnes and landuses.
#'
#' @param fert_nrm A file path to vector data or an `sf` object.
#' @param fert_landuses A file path to raster file or  a `RasterLayer`
#'   object.
#' @param outfile Character. Output raster file path. If not provided,
#'   `RasterLayer` will be returned to R.
#' @param return_rast Logical. Should the `RasterLayer` be returned to R?
#'   Ignored if `outfile` is not provided.
#' @return If `outfile` is specified, the resulting `RasterLayer` is
#'   saved as to that path. If `return_rast` is `TRUE` or
#'   `outfile` is not specified, the resulting `RasterLayer` is
#'   returned, otherwise `NULL` is returned invisibly.
#' @importFrom sf read_sf
#' @importFrom raster raster writeRaster
#' @importFrom fasterize fasterize
#' @export

fertiliser_weight <- function(fert_nrm, fert_landuses, outfile,
  return_rast=FALSE) {

  # Load NRM sf object if path provided
  if(is.character(fert_nrm)) {
    fert_nrm <- sf::read_sf(fert_nrm)
  }

  # Load raster if path provided
  if(is.character(fert_landuses)) {
    fert_landuses <- raster::raster(fert_landuses)
  }

  # Rasterize NRM layer
  fert_nrm <- fasterize::fasterize(fert_nrm, fert_landuses, field = "Fert_t")

  # Mask non-fertiliser landuses
  out <- fert_nrm * fert_landuses

  # Convert to proportion
  out <- calc_proportion(out)

  if(!missing(outfile)) {
    # Create directory if it does not exist
    if(!dir.exists(dirname(outfile))) {
      dir.create(dirname(outfile), recursive = TRUE)
    }

    # write out raster
    raster::writeRaster(out, outfile, overwrite=TRUE)
  }

  if(isTRUE(return_rast) || missing(outfile)) out else invisible(NULL)

}
