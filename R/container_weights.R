#' Creates a weight by postcode for each major port
#'
#' Creates a weight by postcode for each major port.
#'
#' @param path Character. File path to Microsoft Excel .xls file containing
#'   containers by postcode for each port.
#' @param sheet_nums Integer. Vector of integers signifying the sheet numbers to
#'   read in.
#' @param range A cell range to read from, as described in cell-specification.
#'   Includes typical _Excel_ ranges such as `"B3:D87"`, possibly including the
#'   sheet name like `"Budget!B2:G14"`, and more. Interpreted strictly, even if
#'   the range forces the inclusion of leading or trailing empty rows or
#'   columns.
#' @param postcode_poly Character. File path to postcode vector dataset (e.g.
#'   ESRI Shapefile).
#' @param na Character vector of strings to interpret as missing values. By
#'   default, `readxl` treats blank cells as missing data.
#' @param outfile Character. Name of shapefile where output will be saved. If
#'   not provided, [`SpatVector`] object will be returned to R.
#' @param return_vect Logical. Should the [`SpatVector`] object be returned to
#'   R? Ignored if `outfile` is not provided.
#' @details For the purposes of this analysis missing data (i.e. NAs) will be
#'   treated as zeroes.
#' @return A [`SpatVector`] object if `return_vect` is `TRUE` or if `outfile` is
#'   missing; `NULL` otherwise.
#' @importFrom readxl excel_sheets read_excel
#' @importFrom purrr set_names map_df
#' @importFrom dplyr rename mutate group_by summarise ungroup left_join across
#' @importFrom tidyr spread replace_na gather
#' @importFrom terra vect project writeVector subset values
#' @export
container_weights <- function(path, sheet_nums, range = "A7:M2217",
                              postcode_poly, na = c("", "-", "np"), outfile,
                              return_vect=FALSE) {

  x <- suppressMessages(
    readxl::excel_sheets(path) %>%
      .[sheet_nums] %>%
      purrr::set_names(sub("^[0-9. ]+([^-]+).*", "\\1", .)) %>%
      purrr::map_df(~ readxl::read_excel(path = path,
                                         range = range,
                                         sheet = .x,
                                         na = na), .id = "sheet")) %>%
    dplyr::rename(Port =1, Postcode = 2) %>%
    dplyr::mutate(Postcode = formatC(
      Postcode, width = 4, format = "d", flag = "0")
    ) %>%
    tidyr::gather(Month,Containers,-Port,-Postcode) %>%
    dplyr::mutate(Containers = round(
      as.numeric(ifelse(is.na(Containers), 0, Containers))
    ), 0) %>%
    dplyr::group_by(Port, Postcode) %>%
    dplyr::summarise(Containers = sum(Containers)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(Port) %>%
    dplyr::mutate(Containers = Containers/sum(Containers)) %>%
    dplyr::ungroup() %>%
    tidyr::spread(Port, Containers)

  pc <- terra::vect(postcode_poly)
  # terra doesn't play nicely with dplyr, so we pull out the attributes and
  # modify them, and then reassociate them with the spatial geoms.
  pc_dat <- as.data.frame(pc) %>%
    dplyr::left_join(x, by=c('POA_CODE' = 'Postcode')) %>%
    mutate(
      dplyr::across(where(is.numeric), ~tidyr::replace_na(.x, 0))
    )
  terra::values(pc) <- pc_dat # assign attributes back to the geoms

  out <- terra::subset(pc, pc$SQKM > 0) %>%
    terra::project('+init=epsg:3577')

  if(!missing(outfile)) {
    # Create directory if it does not exist
    if(!dir.exists(dirname(outfile))) {
      dir.create(dirname(outfile), recursive = TRUE)
    }
    # write out vector dataset
    terra::writeVector(out, outfile)
  }
  if(isTRUE(return_vect) || missing(outfile)) {
    out
  } else {
    invisible(NULL)
  }
}
