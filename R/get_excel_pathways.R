#' Get pathway data from an Excel file
#'
#' This function reads the "Pathways" sheet from the specified Excel file and
#' returns a list of pathways.
#'
#' @param file The path to the Excel file.
#' @return A list of pathways.
#' @examples
#' get_excel_pathways(
#'   system.file("extdata", "pathways.xlsx", package = "edmaps")
#' )
#' @importFrom dplyr rename mutate
#' @importFrom readxl read_excel
#' @export
get_excel_pathways <- function(file) {
  pathways <- readxl::read_excel(file, sheet = "Pathways") %>%
    dplyr::rename(
      species = `Species`, pathway = `Pathway name`,
      leakage_lower = `Leakage rate lower`, leakage_upper = `Leakage rate upper`,
      viability_lower = `Viability lower`, viability_upper = `Viability upper`,
      weights = `Arrival weights (raster)`
    ) %>%
    dplyr::mutate(
      species = gsub("^\\s+|\\s+$", "", species),
      species = gsub("\\s+", " ", species)
    )

  pathways
}
