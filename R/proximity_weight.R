#' Create a seaport proximity weighting raster
#'
#' Create a weight raster describing distance from one or more points, with
#' negative exponential distance decay.
#'
#' @param pts [`SpatVector`] with point geometry, or a filepath to a spatial
#'   dataset with point geometry. CRS must be defined.
#' @param rast `Raster*` or [`SpatRaster`] object, or a file path to template
#'   raster. The template raster will also be used to mask the output raster so
#'   that NA cell values will be propagated. CRS must be defined. Distance will
#'   be calculated for all non-NA cells.
#' @param beta Numeric. Parameter passed to the exponential function. Distance
#'   to nearest airport is multiplied by this value and exponentiated to give
#'   the relative density of tourists at a location. To generate a distribution
#'   that ensures proportion _p_ of tourists within distance _d_ of nearest
#'   airport, specify `beta=log(p)/d` (e.g. to have 50% of tourists
#'   within 200 km of an airport, use `log(0.5)/200000`) (assuming that either
#'   the map is in long/lat or map units are metres).
#' @param pt_weight An optional vector of weights that correspond to elements of
#'   `pts`. These weights are applied after the negative exponential distance
#'   weighting, by multiplication. E.g., if `pts` is a set of seaports,
#'   `pt_weight` might be a vector of annual vessel counts for each port. When
#'   `method='nearest'`, length of `pt_weight` must be 1. When `method='all'`,
#'   length of `pt_weight` can be 1 (recycled to length of `pts`), or the number
#'   of features in `pts`.
#' @param cell_weight An optional `RasterLayer` or [`SpatRaster`] object, or
#'   path to a raster file, containing cell weights to apply to the weighted
#'   distances. E.g., if `pts` is a set of airports, `cell_weight` could be a
#'   raster of human population density. Must have same geometry as `rast`.
#' @param method Either `'nearest'` (default; only consider distance to the
#'   nearest point in `pts`) or `'all'` (sum weighted distances to all points in
#'   `pts`).
#' @param outfile Character. Output raster file path. If not provided, the
#'   resulting [`SpatRaster`] will be returned to R.
#' @param return_rast Logical. Should the [`SpatRaster`] be returned to R?
#'   Ignored if `outfile` is not provided.
#' @return If `outfile` is specified, the resulting [`SpatRaster`] is saved as
#'   to that path. If `return_rast` is `TRUE` or `outfile` is not specified, the
#'   resulting [`SpatRaster`] is returned, otherwise `outfile` is returned
#'   invisibly. Note that cell weights are divided by their global sum.
#' @importFrom terra cells crs distance init project rast vect writeRaster xyFromCell
#' @importFrom utils read.csv
#' @importFrom methods is
#' @importFrom magrittr %>%
#' @export
proximity_weight <- function(pts, rast, pt_weight, cell_weight, beta,
                             method=c('nearest', 'all'), outfile,
                             return_rast=FALSE) {

  method <- match.arg(method)

  if(is.character(rast) || is(rast, 'Raster')) {
    rast <- terra::rast(rast)
  } else if(!is(rast, 'SpatRaster')) {
    stop('`rast` must be a Raster* or SpatRaster object, or raster file path.',
         call.=FALSE)
  }

  if(!missing(cell_weight)) {
    if(is.character(cell_weight) || is(cell_weight, 'Raster')) {
      cell_weight <- terra::rast(cell_weight)
    } else if(!is(cell_weight, 'SpatRaster')) {
      stop('`cell_weight` must be a Raster* or SpatRaster object, ',
           'or raster file path.', call.=FALSE)
    }
    terra::compareGeom(cell_weight, rast, crs=TRUE, rowcol=TRUE, res=TRUE,
                       stopOnError=TRUE)
  }

  if(is.character(pts)) {
    pts <- terra::vect(pts)
  } else if(!is(pts, 'SpatVector')) {
    stop('`pts` must be a SpatVector or file path to a vector file.',
         call.=FALSE)
  }

  if((terra::crs(pts) == '') || (terra::crs(rast) == '')) {
    stop('CRS must be defined for both `pts` and `rast`.')
  } else if(terra::crs(pts) != terra::crs(rast)) {
    pts <- terra::project(pts, terra::crs(rast))
  }


  if(!missing(pt_weight)) {
    if(length(pt_weight) == 1) {
      pt_weight <- rep(pt_weight, length(pts))
      # ^ this is redundant if we divide by global sum before returning (i.e.
      # should just not do the weighting step)
    } else if(length(pt_weight) != nrow(pts)) {
      stop('Length of `pt_weight` vector must be 1 or equal to `length(pts)`.',
           call.=FALSE)
    }
  }

  # Create directory if it does not exist
  if(!missing(outfile) && !dir.exists(dirname(outfile))) {
    dir.create(dirname(outfile), recursive = TRUE)
  }

  not_na <- terra::cells(!is.na(rast), 1)[[1]]
  cells <- terra::xyFromCell(rast, not_na) %>%
    terra::vect(crs=terra::crs(rast))
  dists <- terra::distance(cells, pts)/1000
  weight <- exp(dists*beta)

  if(method=='nearest') {
    nearest <- max.col(-dists, ties.method='first')
    weight <- weight[cbind(1:nrow(weight), nearest)]
    if(!missing(pt_weight)) {
      weight <- weight * pt_weight[nearest]
    }
  } else { # method=='all'
    if(!missing(pt_weight)) {
      weight <- rowSums(sweep(weight, 2, pt_weight, `*`))
    }
  }
  out <- terra::init(rast, NA)
  out[not_na] <- weight

  if(!missing(cell_weight)) out <- out * cell_weight

  # Convert to proportion
  out <- calc_proportion(out)

  if(!missing(outfile)) {
    # Create directory if it does not exist
    if(!dir.exists(dirname(outfile))) {
      dir.create(dirname(outfile), recursive=TRUE)
    }
    terra::writeRaster(out, outfile, overwrite=TRUE)
  }

  if(isTRUE(return_rast) || missing(outfile)) out else invisible(outfile)
}
