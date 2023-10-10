#' Fit and project range bag model
#'
#' Fit and project range bag model.
#'
#' @param occurrence_data `sf` object, `data.frame` or character
#'   path to a csv file containing occurrence coordinates (must contain
#'   columns named "Latitude" and "Longitude").
#' @param bioclim `SpatRaster` object with layers named using the convention
#'   "bionn" where _nn_ gives the bioclim variable number.
#' @param n_dims Integer. The number of dimensions ranges to bag.
#' @param n_models Integer. The number of bootstrapped model ensembles to run.
#' @param p Numeric between 0 and 1. The proportion of occurrence records to
#'   include in bootstrapping .
#' @param exclude_vars Character vector. A vector of bioclim variables to
#'   exclude from analysis. Default is `NULL`.
#' @param outfile Character. Output raster file path. Parent directory will be
#'   created recursively if required. If missing, the `SpatRaster` will be
#'   returned in R.
#' @return A `SpatRaster` of model predictions is written to `outfile` if
#'   provided, and returned to R otherwise. The raster's extent, resolution and
#'   CRS are taken from `bioclim`. Cell values give the fraction of bootstrapped
#'   models for which the cell's environment fell within the species' modelled
#'   climate envelope.
#' @references This function is a modified version of the `rb` function
#'   provided in Drake, J.M. & Richards, R.L. (2019)
#'   \href{https://doi.org/10.5061/dryad.g5p7d1c}{Data from: Estimating
#'   environmental suitability.} Dryad, Dataset, doi:10.5061/dryad.g5p7d1c.
#'
#'   See also: Drake, J.M. (2015)
#'   \href{https://doi.org/10.1098/rsif.2015.0086}{Range bagging: a new method
#'   for ecological niche modelling from presence-only data.} \emph{Journal of
#'   the Royal Society Interface}, 12(107), 20150086.
#'   doi:https://doi.org/10.1098/rsif.2015.0086.
#' @importFrom dplyr everything filter select
#' @importFrom geometry convhulln
#' @importFrom sf st_transform
#' @importFrom stats na.omit setNames
#' @importFrom terra as.data.frame cells rast vect writeRaster
#' @importFrom utils read.csv
#' @importFrom magrittr %>%
#' @export
range_bag <- function(occurrence_data, bioclim, n_dims = 2, n_models = 100,
                      p = 0.5, exclude_vars = NULL, outfile, quiet=FALSE) {

  range_bag_fit <- function(fit_data, n_models, dimensions, p) {
    models <- list()
    n <- dim(fit_data)
    for(i in 1:n_models){
      vars <- sample.int(n[2], size=dimensions, replace=FALSE)
      x0 <- fit_data[, vars]

      if(dimensions==1) {
        x1 <- x0[sample(n[1], ceiling(p*n[1]), replace=FALSE)]
        models[[i]] <- list(vars=vars, endpoints=c(min(x1), max(x1)), data=x1)
      }
      else{
        x1 <- x0[sample(n[1], ceiling(p*n[1]), replace=FALSE),]
        idx <- unique(as.vector(geometry::convhulln(x1, options='Pp')))
        endpoints <- x1[idx,]
        models[[i]] <- list(vars=vars, endpoints=as.matrix(endpoints),
                            data=unique(x1))
      }
    }
    return(models)
  }

  range_bag_pred <- function(models, new_data) {

    n_models <- length(models)
    dimensions <- ifelse(is.null(dim(models[[1]]$endpoints)), 1,
                         dim(models[[1]]$endpoints)[2])
    n <- dim(new_data)
    out <- numeric(n[1])
    for(i in 1:n_models){
      if(!quiet) cat(sprintf('\r%.02f%%', 100*i/n_models))
      if(dimensions==1){
        test.pts <- (models[[i]]$endpoints[1] < new_data[,models[[i]]$vars]) &
          (new_data[,models[[i]]$vars] < models[[i]]$endpoints[2])
        out <- out + test.pts
      } else{
        test.dat <- new_data[,models[[i]]$vars]
        ep <- models[[i]]$endpoints
        # sort vertices in clockwise order (https://stackoverflow.com/a/48250095/489704)
        ep <- ep[order(-1 * atan2(ep[, 2] - mean(range(ep[, 2])), ep[, 1] -
                                    mean(range(ep[, 1])))),]
        tryCatch({test.pts <- pointsInPoly(test.dat, ep)}, error=function(e){})
        out <- out + test.pts
      }
    }
    return(out/n_models)
  }

  if(missing(bioclim)) stop('`bioclim` must be provided.')
  if(!is(bioclim, 'SpatRaster')) stop('`bioclim` must be a SpatRaster.')
  if(any(!grepl('^bio\\d+$', names(bioclim)))) {
    stop('Invalid `bioclim` layer names - see ?range_bag')
  }
  names(bioclim) <- sprintf('bio%02d', as.numeric(sub('bio', '', names(bioclim))))

  if(!is.null(exclude_vars)) {
    if(!all(exclude_vars %in%  names(bioclim))) {
      stop(paste("Only the following variables can be excluded:", names(bioclim)))
    }
    bioclim <- bioclim[[setdiff(names(bioclim), exclude_vars)]]
  }

  bioclim_df <- terra::as.data.frame(bioclim, xy=TRUE, cells=TRUE, na.rm=TRUE)

  if(is.character(occurrence_data)) {
    occurrence_data <- utils::read.csv(occurrence_data)
  }

  if("sf" %in% class(occurrence_data)) {
    if(is.na(sf::st_crs(occurrence_data))) {
      sf::st_crs(occurrence_data) <- 4326
    } else {
      occurrence_data <- sf::st_transform(occurrence_data, crs = 4326)
    }
    occurrence_data <- terra::vect(occurrence_data)
  } else {
    names(occurrence_data) <- tolower(names(occurrence_data))
    occurrence_data <- terra::vect(
      occurrence_data, geom=c('longitude', 'latitude'), crs='EPSG:4326'
    )
  }

  # Reduces points to 1 per grid cell
  occ_cells <- unique(terra::cells(bioclim, occurrence_data)[, 'cell'])
  occ_env <- dplyr::filter(bioclim_df, cell %in% occ_cells)[, -(1:3)]
  # ^ drop cell number and coords

  models <- range_bag_fit(occ_env, n_models=n_models, dimensions=n_dims, p=p)

  pred_cells <- terra::cells(bioclim) # non-NA cells
  pred_data <- as.matrix(dplyr::filter(bioclim_df, cell %in% pred_cells))

  out <- suppressWarnings(
    terra::rast(cbind(
      pred_data[, 2:3],
      range_bag_pred(models = models, new_data = pred_data[, -(1:3)])
    ), type='xyz', crs='EPSG:4326')
  )
  if(!missing(outfile)) {
    # Create directory if it does not exist
    if(!dir.exists(dirname(outfile))) {
      dir.create(dirname(outfile), recursive = TRUE)
    }
    terra::writeRaster(out, outfile, overwrite=TRUE)
  } else {
    out
  }
}
