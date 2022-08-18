#' Create a fertiliser weight raster
#'
#' Create a fertiliser weight raster as a function of estimated nrm fertiliser
#' tonnes and landuses.
#'
#' @param fert_nrm  A [`SpatVector`], `sf` object, `Spatial` (sp) object, or
#'   file path to vector dataset.
#' @param fert_landuses A [`SpatRaster`], `RasterLayer`, or file path to
#'   raster file.
#' @param outfile Character. Output raster file path. If not provided, the
#'   resulting [`SpatRaster`] will be returned to R.
#' @param return_rast Logical. Should the [`SpatRaster`] be returned to R?
#'   Ignored if `outfile` is not provided.
#' @return If `outfile` is specified, the resulting [`SpatRaster`] is saved as
#'   to that path. If `return_rast` is `TRUE` or `outfile` is not specified, the
#'   resulting [`SpatRaster`] is returned, otherwise `NULL` is returned
#'   invisibly.
#' @importFrom terra rast writeRaster vect rasterize
#' @importFrom methods is
#' @export
fertiliser_weight <- function(fert_nrm, fert_landuses, outfile,
  return_rast=FALSE) {

  if(is.character(fert_nrm) || is(fert_nrm, 'Spatial' || is(fert_nrm, 'sf'))) {
    fert_nrm <- terra::vect(fert_nrm)
  } else if(!is(fert_nrm, 'SpatVector')) {
    stop('fert_nrm must be a SpatVector, a Spatial (sp) object, an sf object, ',
         'or a file path to a vector dataset.')
  }

  if(is.character(fert_landuses) || is(fert_landuses, 'RasterLayer')) {
    fert_landuses <- terra::rast(fert_landuses)
  } else if(!is(fert_landuses, 'SpatRaster') || dim(fert_landuses)[3] > 1) {
    stop('fert_landuses must be a RasterLayer, single-layered SpatRaster, ',
    'or file path to a raster file.')
  }

  # Rasterize NRM layer
  fert_nrm <- terra::rasterize(fert_nrm, fert_landuses, field = "Fert_t")

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
    terra::writeRaster(out, outfile, overwrite=TRUE)
  }

  if(isTRUE(return_rast) || missing(outfile)) out else invisible(NULL)

}
