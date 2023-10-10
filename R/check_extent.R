#' Check and adjust the extent of a raster
#'
#' This function checks whether longitudinal and latitudinal ranges are
#' multiples of `res`, and if not snaps the extent in or out according to
#' `snap`.
#' @param extent The extent to check.
#' @param res The resolution to check against.
#' @param snap The direction to snap the extent. Can be "out" or "in".
#' @return The adjusted extent.
#' @examples
#' check_extent(extent = c(0, 1, 0, 1), res = 0.5, snap = "out")
#' check_extent(extent = c(0, 1, 0, 1), res = 0.3, snap = "out")
#' check_extent(extent = c(0, 1, 0, 1), res = 0.3, snap = "in")
#' @importFrom terra ext
#' @export
check_extent <- function(extent, res, snap = c("out", "in")) {
  snap <- match.arg(snap)
  if (!is(extent, "SpatExtent")) extent <- terra::ext(extent)
  if (length(unique(res)) > 1) {
    stop("Longitudinal and latitudinal resolution must be equal.")
  }
  x_offset <- diff(extent[1:2]) %% res
  y_offset <- diff(extent[3:4]) %% res
  e <- switch(snap,
    `out` = c(
      extent[1], extent[2] + ifelse(x_offset == 0, 0, res - x_offset),
      extent[3], extent[4] + ifelse(y_offset == 0, 0, res - y_offset)
    ),
    `in` = c(extent[1], extent[2] - x_offset, extent[3], extent[4] - y_offset)
  )
  if (any(e != extent[])) {
    warning(
      call. = FALSE,
      sprintf("Extent snapped %s to %s.", snap, paste(e, collapse = ", "))
    )
  }
  e
}
