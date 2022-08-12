#' Convert an alpha hull into an sf object
#'
#' Convert an alphahull ahull object into an sf object.
#'
#' @param xy A two-column matrix of x, y coordinates.
#' @param alpha Alpha parameter for calculating the hull.
#' @return A [`SpatVector`] object.
#' @references Adapted from ConR:::.alpha.hull.poly and ConR:::.alpha.hull.poly.
#' @importFrom alphahull ahull anglesArc
#' @importFrom terra vect aggregate buffer fillHoles set.crs
#' @keywords internal
#' @examples
#' h <- alphahull(matrix(runif(100), ncol=2), 0.5)
#' plot(h, col=2)
alphahull <- function(xy, alpha) {
  ah <- alphahull::ahull(xy, alpha = alpha)
  arcs <- as.data.frame(ah$arcs)

  ah_poly <- mapply(function(v.x, v.y, theta, r, c1, c2) {
    angles <- alphahull::anglesArc(c(v.x, v.y), theta)
    seqang <- seq(angles[1], angles[2], length=1000)
    terra::vect(cbind(c1 + r*cos(seqang), c2 + r*sin(seqang)), 'lines',
                crs='+init=epsg:4326')
    # ^ crs temporarily assigned to permit buffering
  }, arcs$v.x, arcs$v.y, arcs$theta, arcs$r, arcs$c1, arcs$c2,
  SIMPLIFY=FALSE) %>%
    terra::vect() %>% # combine individual arcs into single SpatVector
    terra::aggregate() %>% # aggregate to a single feature
    terra::buffer(width=0.1) # convert lines to polys

  out <- terra::fillHoles(ah_poly) # fill the hole created by buffering
  terra::set.crs(out, '') # remove the temporary crs (in-place modification)
}
