#' Convert an alpha hull into an sf object
#'
#' Convert an alphahull ahull object into an sf object.
#'
#' @param xy A two-column matrix of x, y coordinates.
#' @param alpha Alpha parameter for calculating the hull.
#' @return An \code{ahull} object.
#' @references Adapted from ConR:::.alpha.hull.poly and ConR:::.alpha.hull.poly.
#' @importFrom alphahull ahull anglesArc
#' @importFrom methods slot
#' @importFrom raster buffer
#' @importFrom sf st_as_sf
#' @importFrom sp CRS Line Lines Polygons proj4string SpatialLines SpatialLinesDataFrame SpatialPolygons
#' @keywords internal
alphahull_sf <- function(xy, alpha) {
  ah <- alphahull::ahull(xy, alpha = alpha)
  arcs <- as.data.frame(ah$arcs)

  ah_spldf <- mapply(function(v.x, v.y, theta, r, c1, c2) {
    angles <- alphahull::anglesArc(c(v.x, v.y), theta)
    seqang <- seq(angles[1], angles[2], length=1000)
    sp::Line(cbind(c1 + r*cos(seqang), c2 + r*sin(seqang)))
  }, arcs$v.x, arcs$v.y, arcs$theta, arcs$r, arcs$c1, arcs$c2,
  SIMPLIFY=FALSE) %>%
    sp::Lines(ID=1) %>%
    list %>%
    sp::SpatialLines(
      proj4string=sp::CRS(as.character(NA), doCheckCRSArgs=TRUE)
    ) %>%
    sp::SpatialLinesDataFrame(data.frame(id=1), match.ID=FALSE)

  ah_poly <- raster::buffer(ah_spldf, width = 0.00001)
  # ^ required to convert lines to polys

  pols <- methods::slot(ah_poly, "polygons")
  holes <- lapply(pols, function(x)
    sapply(methods::slot(x, "Polygons"), slot, "hole"))
  not_holes <- lapply(1:length(pols), function(i)
    methods::slot(pols[[i]], "Polygons")[!holes[[i]]])
  IDs <- row.names(ah_poly)
  ah_poly_not_holes <- sp::SpatialPolygons(lapply(1:length(not_holes), function(i)
    sp::Polygons(not_holes[[i]], ID = IDs[i])),
    proj4string = sp::CRS(sp::proj4string(ah_poly), doCheckCRSArgs = TRUE))
  return(sf::st_as_sf(ah_poly_not_holes))
}
