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
#' @param postcodes Character. File path to postcode vector dataset (e.g. ESRI
#'   Shapefile).
#' @param port_data Character. Path to csv file containing Port Names, Port
#'   Codes, Longitude, Latitude and Container volumes. Column names must be (in
#'   this order): Name, PortCode, Longitude, Latitude, Count.
#' @param na Character vector of strings to interpret as missing values. By
#'   default, `readxl` treats blank cells as missing data.
#' @param template_raster [`SpatRaster`] or `RasterLayer` object, or a file
#'   path to a raster file. This is used to define the extent and resolution of
#'   output. Must be specified in CRS EPSG:3577.
#' @param outfile Character. Name of shapefile where output will be saved. If
#'   not provided, [`SpatVector`] object will be returned to R.
#' @param return_rast Logical. Should the [`SpatRaster`] object be returned to
#'   R? Ignored if `outfile` is not provided.
#' @details For the purposes of this analysis missing container counts (i.e.
#'   NAs) are treated as zeroes.
#' @return A [`SpatRaster`] object if `return_rast` is `TRUE` or if `outfile` is
#'   missing; otherwise, `outfile` is returned invisibly.
#' @importFrom dplyr across bind_rows filter group_by left_join mutate n rename summarise ungroup
#' @importFrom readxl excel_sheets read_excel
#' @importFrom terra merge project rast rasterize subset values vect writeRaster
#' @importFrom tidyr pivot_longer pivot_wider replace_na
#' @importFrom utils read.csv
#' @importFrom magrittr %>%
#' @export
container_weight <- function(path, sheet_nums, range = "A7:M2217", postcodes,
                              port_data, na = c("", "-", "np"), template_raster,
                              outfile, return_rast=FALSE) {

  # Load template_raster
  if(is.character(template_raster) || is(template_raster, 'Raster')) {
    template_raster <- terra::rast(template_raster)
  } else if(!is(template_raster, 'SpatRaster')) {
    stop('template_raster must be a SpatRaster or RasterLayer object, ',
         'or a file path to a raster file.')
  }

  sheets <- readxl::excel_sheets(path)[sheet_nums]
  x <- lapply(sheets, function(x) {
    suppressMessages(
      readxl::read_excel(path=path, range=range, sheet=x, na=na)
    )
  }) %>%
    setNames(sub("^[0-9. ]+([^-]+).*", "\\1", sheets)) %>%
    dplyr::bind_rows(.id = "Port") %>%
    dplyr::rename(Postcode=2) %>%
    tidyr::pivot_longer(-c('Port', 'Postcode'), names_to='Month',
                        values_to='Containers') %>%
    dplyr::mutate(Postcode=sprintf('%04d', Postcode),
                  Containers=round(ifelse(is.na(Containers), 0, Containers),
                                   0)) %>%
    dplyr::group_by(Port, Postcode) %>%
    dplyr::summarise(Containers = sum(Containers), .groups='drop_last') %>%
    dplyr::mutate(Containers = Containers/sum(Containers)) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_wider(names_from='Port', values_from='Containers')

  pc <- terra::vect(postcodes)
  # terra doesn't play nicely with dplyr, so we pull out the attributes and
  # modify them, and then reassociate them with the spatial geoms.
  pc_dat <- as.data.frame(pc) %>%
    dplyr::left_join(x, by=c('POA_CODE' = 'Postcode')) %>%
    dplyr::mutate(
      dplyr::across(where(is.numeric), ~tidyr::replace_na(.x, 0))
    )
  terra::values(pc) <- pc_dat # assign attributes back to the geoms

  weight <- terra::subset(pc, pc$SQKM > 0) %>%
    terra::project('+init=epsg:3577')
  weight$POA_CODE <- as.numeric(weight$POA_CODE) # in prep for rasterising

  # Subset port data to ports present in container weights
  port_data <- utils::read.csv(port_data) %>%
    dplyr::filter(Name %in% names(weight)) %>%
    dplyr::mutate(Name = as.character(Name))

  # Multiply proportional postcode distribution volumes by port container
  # arrival volumes, and sum resulting postcode arrivals across source ports
  weight$total <-
    rowSums(sweep(as.data.frame(weight[, port_data$Name]), 2,
                  port_data$Count, `*`))

  # Convert total postcode arrivals to a proportion of their sum
  weight$total <- weight$total/sum(weight$total, na.rm=TRUE)

  # Rasterise postcodes
  postcode_rast <- terra::rasterize(
    weight, y=template_raster, field = "POA_CODE"
  )

  postcode_n <- as.data.frame(postcode_rast) %>%
    na.omit %>%
    dplyr::group_by(POA_CODE) %>%
    dplyr::summarise(n_cells=dplyr::n())

  weight <- terra::merge(weight, postcode_n, by='POA_CODE')
  weight$total_per_cell <- weight$total/weight$n_cells

  # Rasterize postcode arrivals
  weight <- terra::rasterize(weight, y=template_raster, field='total_per_cell')

  # Rescale due to missing postcodes not being rasterised (not overlaying cell
  # centres).
  weight <- calc_proportion(weight)

  if(!missing(outfile)) {
    # Create directory if it does not exist
    if(!dir.exists(dirname(outfile))) {
      dir.create(dirname(outfile), recursive = TRUE)
    }
    # write out raster dataset
    terra::writeRaster(weight, outfile)
  }
  if(isTRUE(return_rast) || missing(outfile)) {
    weight
  } else {
    invisible(outfile)
  }
}
