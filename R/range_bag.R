#' Fit and project range bag model
#' 
#' Fit and project range bag model.
#' 
#' @param occurrence_data \code{sf} object, \code{data.frame} or character 
#'   path to a csv file containing occurrence coordinates (must contain 
#'   columns named "Latitude" and "Longitude").
#' @param bioclim_dir Path. Path to directory containing WorldClim raster data.
#' @param n_dims Integer. The number of dimensions ranges to bag.
#' @param n_models Integer. The number of bootstrapped model ensembles to run.
#' @param p Numeric between 0 and 1. The proportion of occurrence records to
#'   include in bootstrapping .
#' @param exclude_vars Character vector. A vector of bioclim variables to
#'   exclude from analysis. Default is \code{NULL}.
#' @param outfile Character. Output raster file path. Parent directory will be 
#'   created recursively if required. If \code{NULL}, the \code{RasterLayer} 
#'   will be returned in R.
#' @return A \code{RasterLayer} of model predictions is written to 
#'   \code{outfile} if provided, and returned to R otherwise. The raster's 
#'   extent, resolution and CRS are taken from the raster data in 
#'   \code{bioclim_dir}. Cell values give the fraction of bootstrapped models 
#'   for which the cell's environment fell within the species' modelled 
#'   climate envelope. 
#' @references This function is a modified version of the \code{rb} function
#'   provided in Drake, J.M. & Richards, R.L. (2019)
#'   \href{https://doi.org/10.5061/dryad.g5p7d1c}{Data from: Estimating
#'   environmental suitability.} Dryad, Dataset, doi:10.5061/dryad.g5p7d1c.
#'   
#'   See also: Drake, J.M. (2015)
#'   \href{https://doi.org/10.1098/rsif.2015.0086}{Range bagging: a new method
#'   for ecological niche modelling from presence-only data.} \emph{Journal of
#'   the Royal Society Interface}, 12(107), 20150086.
#'   doi:https://doi.org/10.1098/rsif.2015.0086.
#' @importFrom geometry tsearchn convhulln delaunayn
#' @importFrom raster stack dropLayer crop rasterFromXYZ writeRaster as.data.frame
#' @importFrom utils read.csv
#' @importFrom sf as_Spatial st_transform
#' @importFrom sp coordinates proj4string CRS
#' @importFrom stats na.omit
#' @importFrom rnaturalearth ne_countries
#' @importFrom dplyr filter
#' @importFrom magrittr "%>%"
#' @export
range_bag <- function(occurrence_data, bioclim_dir, n_dims = 2, n_models = 100,
  p = 0.5, exclude_vars = NULL, outfile) {
  
  # SUB FUNCTIONS
  # Fit function
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
        x1 <- x0[sample(n[1],ceiling(p*n[1]), replace=FALSE),]
        idx <- unique(as.vector(geometry::convhulln(x1, options='Pp')))
        endpoints <- x1[idx,]
        models[[i]] <- list(vars=vars, endpoints=endpoints, data=unique(x1))
      }
    }
    return(models)
  }
  # Prediction function
  range_bag_pred <- function(models, new_data) {
    
    n_models <- length(models)
    dimensions <- ifelse(is.null(dim(models[[1]]$endpoints)), 1, 
                         dim(models[[1]]$endpoints)[2])
    n <- dim(new_data)
    out <- numeric(n[1])
    for(i in 1:n_models){
      if(dimensions==1){
        test.pts <- (models[[i]]$endpoints[1] < new_data[,models[[i]]$vars]) & 
          (new_data[,models[[i]]$vars] < models[[i]]$endpoints[2])
        out <- out + test.pts
      } else{
        test.dat <- as.matrix(new_data[,models[[i]]$vars])
        tri.pts <- geometry::tsearchn(
          as.matrix(models[[i]]$data), 
          geometry::delaunayn(models[[i]]$data), test.dat)
        test.pts <- !is.na(tri.pts$p[,1])
        out <- out + test.pts      
      }
    }
    return(out/n_models)
  }
  
  bioclim_vars <- c("bio01", "bio02", "bio03", "bio04",
                    "bio05", "bio06", "bio07", "bio08",
                    "bio09", "bio10", "bio11", "bio12",
                    "bio13", "bio14", "bio15", "bio16",
                    "bio17", "bio18",  "bio19") 
  
  if(!is.null(exclude_vars) && !all(exclude_vars %in%  bioclim_vars)) {
    stop(paste("Only the following variables can be excluded:", 
               bioclim_vars))
  }
  
  bioclim_stack <- raster::stack(list.files(bioclim_dir, full.names=TRUE, 
                                            pattern="\\.tif$"))
  
  if(!is.null(exclude_vars)) {
    id <- which(bioclim_vars %in% exclude_vars)
    bioclim_stack <- raster::dropLayer(bioclim_stack, id)
  }
  if(is.character(occurrence_data)) {
    locs <- utils::read.csv(occurrence_data)
  } else {
    locs <- occurrence_data
  }
  
  if(any(("sf") %in% class(locs))) {
    locs <- suppressMessages(
      sf::st_transform(
        locs, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    ) %>%
      sf::as_Spatial(.)
  } else {
    locs <- locs %>%
      {names(.) <- tolower(names(.)); .} %>%
      {sp::coordinates(.) <- c("longitude", "latitude"); .} %>%
      {sp::proj4string(.) <- 
        sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"); .}
  }
  
  
  # Reduces points to 1 per grid cell
  loc_env <- stats::na.omit(
    raster::as.data.frame(
      bioclim_stack[unique(raster::cellFromXY(bioclim_stack, locs))]
    )
  )
  models <- range_bag_fit(loc_env, n_models = n_models, dimensions = n_dims, 
                          p = p)
  
  world_map <- rnaturalearth::ne_countries(returnclass = "sf") %>%
    dplyr::filter(name != "Antarctica")
  
  pred_data <- stats::na.omit(raster::as.data.frame(
    bioclim_stack %>%
      raster::crop(., world_map), xy=TRUE))
  
  out <- suppressWarnings(
    raster::rasterFromXYZ(
      cbind(pred_data[, 1:2], 
            range_bag_pred(models = models, new_data = pred_data[, -(1:2)])), 
      crs = '+init=epsg:4326')
  )
  if(!missing(outfile)) {
    # Create directory if it does not exist
    if(!dir.exists(dirname(outfile))) {
      dir.create(dirname(outfile), recursive = TRUE)
    }
    raster::writeRaster(out, outfile, overwrite=TRUE)
  } else {
    out
  }
}
