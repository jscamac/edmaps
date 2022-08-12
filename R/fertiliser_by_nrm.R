#' Create fertiliser by nrm sf object
#'
#' Create fertiliser by nrm sf object.
#'
#' @param abs_data Character. File path to ABS .csv file.
#' @param nrm_shapefile Character. File path to NRM shapefile.
#' @param outfile Character. Name of shapefile (or other vector data format
#'   supported by OGR) where output will be saved. If not provided, `sf` object
#'   will be returned to R.
#' @param return_vect Logical. Should the [`SpatVector`] object be returned to
#'   R? Ignored if `outfile` is not provided.
#' @return If `return_vect` is `TRUE` or `outfile` is missing, a [`SpatVector`]
#'   object will be returned. Otherwise, the vector dataset is written to
#'   `outfile` and NULL is returned invisibly.
#' @importFrom readr read_csv cols_only
#' @importFrom dplyr mutate filter group_by summarise ungroup select
#' @importFrom terra vect project writeVector
#' @export
fertiliser_by_nrm <- function(abs_data, nrm_shapefile, outfile,
  return_vect=FALSE) {

  fert <- readr::read_csv(abs_data,
                         col_names = c("NRM_ID",
                                       "Region",
                                       "Item_code",
                                       "Item",
                                       "Estimate",
                                       "Estimate_stderr",
                                       "N_businesses",
                                       "N_businesses_stderr"),
                         na = c("", "NA", "np"),
                         readr::cols_only(NRM_ID = "c",
                                          Region = "c",
                                          Item = "c",
                                          Estimate ="c"),
                         skip = 5) %>%
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
    dplyr::summarise(Fert_t = sum(Estimate, na.rm=TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::select(NRM_ID, Fert_t)

  # ABS lumped some NRMs so need to account for that
  nrm <- terra::vect(nrm_shapefile)
  nrm$NRM_ID <- ifelse(
    nrm$NRM_CODE16 %in% c(402, 406), 402406,
    ifelse(nrm$NRM_CODE16 %in% c(801, 110), 110801, nrm$NRM_CODE16))

  out <- nrm %>%
    merge(fert, by="NRM_ID", all.x=TRUE) %>%
    terra::project('+init=epsg:3577')

  if(!missing(outfile)) {
    # Create directory if it does not exist
    if(!dir.exists(dirname(outfile))) {
      dir.create(dirname(outfile), recursive = TRUE)
    }
    # write out shape file
    terra::writeVector(out, outfile, quiet = TRUE)
  }

  if(isTRUE(return_vect) || missing(outfile)) out else invisible(NULL)

}
