#' Calculate median of raster objects
#'
#' Calculate the cellwise median of two or more Raster* objects, by layer.
#'
#' @param files A vector of file paths to (optionally multiband) raster files.
#' @param outfile Character. Output raster file path. If not provided, object
#'   will be returned to R. Directory will be created recursively if it does
#'   not exist.
#' @return If `outfile` is specified, the resulting raster is saved as a
#'   geotiff to that path and NULL is returned. If `outfile` is not
#'   specified the resulting raster is returned.
#' @importFrom raster stack overlay writeRaster
#' @export
calculate_median <- function(files, outfile) {
  ss <- lapply(files, raster::stack)
  result <- do.call(function(...) raster::overlay(..., fun=median), ss)
  if(!dir.exists(dirname(outfile))) dir.create(dirname(outfile), recursive=TRUE)
  if(!missing(outfile)) {
    raster::writeRaster(result, outfile)
    return(NULL)
  } else {
    result
  }
}
