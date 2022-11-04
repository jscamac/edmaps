#' Create a fertiliser weight raster
#'
#' Create a fertiliser weight raster as a function of estimated nrm fertiliser
#' tonnes and landuses.
#'
#' @param abs_data Character. File path to ABS .csv file.
#' @param nrm Character. File path to Natural Resource Management (NRM)
#'   shapefile.
#' @param fert_landuses A [`SpatRaster`], `RasterLayer`, or file path to
#'   raster file.
#' @param outfile Character. Output raster file path. If not provided, the
#'   resulting [`SpatRaster`] will be returned to R.
#' @param return_rast Logical. Should the [`SpatRaster`] be returned to R?
#'   Ignored if `outfile` is not provided.
#' @return If `outfile` is specified, the resulting [`SpatRaster`] is saved as
#'   to that path. If `return_rast` is `TRUE` or `outfile` is not specified, the
#'   resulting [`SpatRaster`] is returned, otherwise `outfile` is returned
#'   invisibly.
#' @importFrom dplyr filter group_by mutate select summarise
#' @importFrom readr cols_only read_csv
#' @importFrom terra merge project rast rasterize vect writeRaster
#' @importFrom methods is
#' @importFrom magrittr %>%
#' @export
fertiliser_weight <- function(abs_data, nrm, fert_landuses, outfile,
                              return_rast=FALSE) {

  if(is.character(nrm)) {
    nrm <- terra::vect(nrm)
  } else if(!is(nrm, 'SpatVector')) {
    stop('`nrm` must be a SpatVector or file path to a vector file.')
  }

  if(is.character(fert_landuses) || is(fert_landuses, 'RasterLayer')) {
    fert_landuses <- terra::rast(fert_landuses)
  } else if(!is(fert_landuses, 'SpatRaster') || dim(fert_landuses)[3] > 1) {
    stop('fert_landuses must be a RasterLayer, single-layered SpatRaster, ',
         'or file path to a raster file.')
  }

  fert <- readr::read_csv(
    abs_data,
    col_names = c("NRM_ID", "Region", "Item_code", "Item", "Estimate",
                  "Estimate_stderr", "N_businesses", "N_businesses_stderr"),
    na = c("", "NA", "np"),
    readr::cols_only(NRM_ID = "c", Region = "c", Item = "c", Estimate ="c"),
    skip = 5
  ) %>%
    dplyr::mutate(Estimate = as.numeric(gsub(",", "", Estimate))) %>%
    dplyr::filter(Item %in% c(
      "Fertiliser - Nitrate slow release fertiliser - Weight applied (t)",
      "Fertiliser - Urea slow release fertiliser - Weight applied (t)",
      "Fertiliser - Other slow release fertiliser - Weight applied (t)",
      "Fertiliser - Fertiliser use - Single superphosphate - Weight applied (t)",
      "Fertiliser - Fertiliser use - Double and/or triple superphosphate - Weight applied (t)",
      "Fertiliser - Fertiliser use - Other phosphorus-based fertilisers - Weight applied (t)",
      "Fertiliser - Fertiliser use - Muriate of potash and/or sulphate of potash - Weight applied (t)",
      "Fertiliser - Fertiliser use - Urea - Weight applied (t)",
      "Fertiliser - Fertiliser use - Ammonium sulphate - Weight applied (t)",
      "Fertiliser - Fertiliser use - Anhydrous ammonia - Weight applied (t)",
      "Fertiliser - Fertiliser use - Potassium nitrate - Weight applied (t)",
      "Fertiliser - Fertiliser use - Ammonium phosphates - Weight applied (t)",
      "Fertiliser - Fertiliser use - Other nitrogen-based fertilisers - Weight applied (t)",
      "Fertiliser - Fertiliser use - All other fertilisers - Weight applied (t)")) %>%
    dplyr::group_by(NRM_ID, Region) %>%
    dplyr::summarise(Fert_tonnes = sum(Estimate, na.rm=TRUE), .groups='drop') %>%
    dplyr::select(NRM_ID, Fert_tonnes)

  # Combine some NRMs as per ABS
  nrm$NRM_ID <- ifelse(
    nrm$NRM_CODE16 %in% c(402, 406), 402406,
    ifelse(nrm$NRM_CODE16 %in% c(110, 801), 110801, nrm$NRM_CODE16)
  )

  fert_nrm <- terra::merge(nrm, fert, by="NRM_ID", all.x=TRUE) %>%
    terra::project('+init=epsg:3577')

  # Rasterize NRM layer
  fert_nrm <- terra::rasterize(fert_nrm, fert_landuses, field = "Fert_tonnes")

  # Mask non-fertiliser landuses
  weight <- fert_nrm * fert_landuses

  # Convert to proportion
  weight <- calc_proportion(weight)

  if(!missing(outfile)) {
    # Create directory if it does not exist
    if(!dir.exists(dirname(outfile))) {
      dir.create(dirname(outfile), recursive=TRUE)
    }
    terra::writeRaster(weight, outfile, overwrite=TRUE)
  }

  if(isTRUE(return_rast) || missing(outfile)) weight else invisible(outfile)
}
