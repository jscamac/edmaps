#' Disperse pest arrivals
#'
#' Disperses pest arrivals based on input raster.
#' 
#' @param rast Raster object containing dispersal weights.
#' @param n_vectors Numeric. The number of transmission vectors (e.g.
#'   passengers).
#' @param probability Numeric. The probability a vector contains pest.
#' @return A raster object.
#' @export
disperse_arrivals <- function(rast, n_vectors, probability) {
  rast * n_vectors * probability
}
