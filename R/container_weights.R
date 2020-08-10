#' Creates a weight by postcode for each major port
#'
#' Creates a weight by postcode for each major port.
#'
#' @param path Character. File path to xls file containing containers by 
#'   postcode for each port.
#' @param sheet_nums Integer. Vector of integers signifying the sheet numbers 
#'   to read in.
#' @param range A cell range to read from, as described in cell-specification.
#'   Includes typical \emph{Excel} ranges such as \code{"B3:D87"}, possibly 
#'   including the sheet name like \code{"Budget!B2:G14"}, and more. 
#'   Interpreted strictly, even if the range forces the inclusion of leading 
#'   or trailing empty rows or columns.
#' @param postcode_shp Character. File path to postcode shape file.
#' @param na Character vector of strings to interpret as missing values. By
#'   default, \code{readxl} treats blank cells as missing data.
#' @param outfile Character. Name of shapefile where output will be saved. If
#'   not provided, \code{sf} object will be returned to R.
#' @param return_sf Logical. Should the \code{sf} object be returned to R?
#'   Ignored if \code{outfile} is not provided.
#' @details For the purposes of this analysis missing data (i.e. NAs) will be
#'   treated as zeroes.
#' @return An \code{sf} object or shapefile export.
#' @importFrom readxl excel_sheets read_excel
#' @importFrom purrr set_names map_df
#' @importFrom dplyr rename mutate group_by summarise ungroup left_join filter mutate_all
#' @importFrom tidyr spread replace_na gather
#' @importFrom sf read_sf st_transform st_write
#' @export
container_weights <- function(path, sheet_nums, range = "A7:M2217", 
  postcode_shp, na = c("", "-", "np"), outfile, return_sf=FALSE) {

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
  
  out <- sf::read_sf(postcode_shp) %>%
    dplyr::left_join(x, by = c("POA_CODE" = "Postcode")) %>%
    dplyr::filter(SQKM>0) %>%
    dplyr::mutate_all(tidyr::replace_na, replace = 0) %>%
    sf::st_transform(crs = '+init=epsg:3577')

  if(!missing(outfile)) {
    # Create directory if it does not exist
    if(!dir.exists(dirname(outfile))) {
      dir.create(dirname(outfile), recursive = TRUE)
    }
    # write out shape file
    sf::st_write(out, outfile,  quiet = TRUE)
  }
  if(isTRUE(return_sf) || missing(outfile)) {
    out
  } else {
    invisible(NULL)
  }
}

