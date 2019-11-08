#' Estimate distance to nearest Australian international airport
#'
#' Generates a raster proximity map indicating the distance from the center of
#' each pixel to the centre of the nearest pixel containing a major airport. 
#' Airport coordinates are in the source raster.
#' 
#' @param vector_data Character. Path to airport data (available
#'   \href{https://d28rz98at9flks.cloudfront.net/74775/MajorAviationTerminals.gdb.zip}{here}).
#' @param outfile Character. Output raster file path. Directory will be 
#'   created recursively if it does not exist.
#' @param template_raster Optional. \code{Raster*} object or a file path to 
#'  template raster. If this is provided, \code{extent}, \code{res}, and 
#'  \code{crs} will be taken from this raster unless they are also passed to 
#'  this function. If \code{template_raster} is not provided, then 
#'  \code{extent} and \code{res} must be provided. The template raster will 
#'  also be used to mask the output raster so that NA cell values will be 
#'  propagated. 
#' @param extent Either a character path to a raster file, an
#'   \code{\link[raster:extent]{Extent}} object (or an object from which such 
#'   an extent can be extracted), or a numeric vector with four elements 
#'   giving xmin, xmax, ymin, ymax.
#' @param res Numeric or integer vector giving the horizontal and vertical
#'   spatial resolution, in units of \code{crs}. If a single value is given,
#'   it will be used for both horizontal and vertical resolution.
#' @param crs Target coordinate reference system as a PROJ string (character) 
#'   or an object of class CRS.
#' @param airport_codes Numeric. Airport codes to be used as targets in order 
#'   to estimate cell proximity.
#' @param return_rast Logical. Return \code{RasterLayer} to R?
#' @param overwrite Logical. Should \code{outfile} be removed if it already
#'   exists?
#' @return A proximity raster is written to \code{outfile}. If 
#'   \code{return_rast} is \code{TRUE}, the raster object is also returned to 
#'   R, otherwise \code{NULL} is returned invisibly. This function assumes that 
#'   \code{crs} is either an unprojected coordinate system, or that the units of 
#'   \code{crs} are metres. Resulting distances are expressed in kilometres.
#' @importFrom raster raster distance extent crs res writeRaster mask
#' @importFrom gdalUtilities gdal_rasterize
#' @importFrom methods is
#' @export
get_airport_dist <- function(vector_data, outfile, template_raster, extent, 
  res, crs, airport_codes, return_rast = FALSE, overwrite=FALSE) {

  # Create directory if it does not exist
  if(!dir.exists(dirname(outfile))) {
    dir.create(dirname(outfile), recursive = TRUE)
  }
  
  if(!missing(template_raster)) {
    if(is.character(template_raster)) 
      template_raster <- raster::raster(template_raster)
    if(missing(extent)) extent <- raster::extent(template_raster)
    if(missing(res)) res <- raster::res(template_raster)
    if(missing(crs)) crs <- raster::crs(template_raster)
  } else {
    if(missing(extent) || missing(res)) {
      stop('If template_raster is not supplied, both extent and res must be supplied.')
    }
    if(missing(crs)) crs <- NA
    # extract extent from raster provided as file path
    if(is.character(extent)) extent <- raster::extent(raster::raster(extent))
    # extract extent from object
    if(!is(extent, 'Extent')) extent <- raster::extent(extent)
  }
  
  # Prepare SQL script to extract airports of interest
  airports <- paste0("AIRPORTCODE IN (",
                     paste0('\'', airport_codes,'\'', collapse=','),')')
  condition <- paste0('SELECT * FROM MajorAviationTerminals WHERE ', airports)

  # Burn vector data into template raster.
  # Airports of interest are given the value 1
  message('Subsetting and rasterizing airport vector data')
  initialise_raster(outfile=f <- tempfile(fileext='.tif'), extent=extent, 
                    res=res, crs=crs, init=NA, datatype='INT1U')
  gdalUtilities::gdal_rasterize(vector_data, f, burn=1, sql=condition)
  
  # Calculate distance to nearest airport and overwrite outfile
  out <- raster::distance(raster::raster(f), doEdge=TRUE)/1000
  out <- raster::mask(out, template_raster)
  raster::writeRaster(out, filename=outfile, overwrite=overwrite)
  
  if(isTRUE(return_rast)) out else invisible(NULL)
}
