#' Generate a drake plan for a species
#'
#' Generate a drake plan that facilitates reproducible generation of species
#' outputs.
#'
#' @param species The name of the species. This will be used for naming output
#'   files and folders.
#' @param clum_classes An integer vector indicating which ACLUM classes are
#'   considered host plants for \code{species}. Either \code{clum_classes} or
#'   \code{nvis_classes} (or both) must be provided.
#' @param nvis_classes An integer vector indicating which NVIS classes are
#'   considered host plants for \code{species}. Either \code{clum_classes} or
#'   \code{nvis_classes} (or both) must be provided.
#' @param include_abiotic_weight Logical. Should suitability be dependent on
#'   climate? Considered TRUE if \code{climate_suitability_path} is provided,
#'   or if \code{use_gbif} is TRUE.
#' @param climate_suitability_path Optional file path to a raster describing
#'   climatic suitability across the landscape. If provided, the raster must be
#'   have the Australian Albers coordinate system (EPSG:3577), spatial
#'   resolution of 1000 m, and must have xmin = -1888000, xmax = 2122000, 
#'   ymin =-4847000, ymax = -1010000. If not provided and 
#'   \code{include_abiotic_weight} is TRUE, a range bag model will be fit to 
#'   estimate climatic suitability.
#' @param exclude_bioclim_vars Character vector of bioclim variables that 
#'   should not be used when fitting a range bag model (see 
#'   \code{\link{range_bag}}) of climatic suitability. Variables should be 
#'   specified as, e.g., \code{c("bio01", "bio12")}. Ignored if 
#'   \code{climate_suitability_path} is provided.
#' @param include_ndvi Logical. Should biotic suitability be dependent on NDVI?
#' @param pathways A character vector of invasion pathways that should be
#'   included. Can be one or more of: \code{'containers'}, \code{'fertiliser'},
#'   \code{'goods'}, \code{'machinery'}, \code{'mail'}, \code{'nurserystock'},
#'   \code{'residents'}, \code{'torres'}, \code{'tourists'}, \code{'vessels'}.
#' @param aggregated_res A numeric vector of 2 elements, indicating the desired
#'   resolution of aggregated establishment likelihood rasters, in metres.
#' @param make_interactive_maps Logical. Should interactive html maps be 
#'   generated?
#' @param clum_path Path to the ACLUM raster.
#' @param nvis_path Path to the NVIS raster.
#' @param ndvi_path Path to the NDVI raster.
#' @param airport_beta Numeric. Parameter controlling the distribution of
#'   international tourists passengers around international airport. Default is
#'   \code{log(0.5)/200} (i.e. 50\% of passengers within 200km of airport).
#' @param airport_tsi_beta Numeric. Parameter controlling the distribution of
#'   Torres Strait passengers around Cairns airport. Default is
#'   \code{log(0.5)/10} (i.e., 50\% of passengers within 10km of Cairns 
#'   airport).
#' @param port_data_path File path to the marine ports .csv file.
#' @param port_weight_beta Numeric. Defines the decay rate of an exponential
#'   model. In the context of pests entering via the vessel pathway, this
#'   reflects the decrease in the relative likelihood of pest arrival at
#'   locations distant from marine ports. For example, 
#'   \code{prob_weight_beta=log(0.5)/10} would lead to distance-decay that 
#'   leads to 50\% (i.e. \code{0.5}) of establishment likelihood (prior to 
#'   considering other relevant pathways) within a distance of \code{10} map 
#'   units (i.e., 10 kilometres when \code{res} is 1000).
#' @param fertiliser_data_path File path to a csv file containing information
#'   about fertiliser usage by NRM.
#' @param nrm_path File path to a polygon shapefile of NRMs (natural resource
#'   management areas).
#' @param containers_data_path File path to the dataset giving the distribution
#'   of containers by postcode.
#' @param postcode_path File path to postal areas shapefile.
#' @param occurrence_path Path to a .csv file containing occurrence data. Must
#'   include columns \code{Longitude}, \code{Latitude}, and \code{Species}.
#' @param infected_countries A character vector of countries within which the 
#'   \code{species} occurs. Ignored if \code{climate_suitability_path} is
#'   provided.Only one of \code{infected_countries} or \code{cabi_path} should
#'   be provided.
#' @param cabi_path Path to a .csv file downloaded from CABI, indicating the
#'   countries within which the \code{species} occurs. Download links to these
#'   files can be found at the bottom of CABI species datasheet webpages, e.g.
#'   https://www.cabi.org/isc/datasheet/17685. Ignored if 
#'   \code{climate_suitability_path} is provided. Only one of 
#'   \code{infected_countries} or \code{cabi_path} should be provided.
#' @param use_gbif Logical. Should species occurrence records be sourced from 
#'   GBIF? Ignored if \code{climate_suitability_path} is provided.
#' @param gbif_species Character vector. Taxon names to use when querying GBIF.
#'   Ignored if \code{climate_suitability_path} is provided.
#' @param gbif_min_year Integer. The minimum year (\code{yyyy}) to be included 
#'   when downloading GBIF data. Ignored if \code{climate_suitability_path} is 
#'   provided.
#' @param gbif_max_uncertainty Numeric. The maximum permissable coordinate 
#'   uncertainty for GBIF records. Ignored if \code{climate_suitability_path} is 
#'   provided.
#' @param gbif_username TODO
#' @param gbif_password TODO
#' @param manual_check_flagged_records Logical. Should an interactive map be
#'   used for manually checking flagged occurrence records? If \code{TRUE}, the 
#'   user will have the opportunity to select dubious points (i.e. occurrences 
#'   in countries for which CABI has no record of the species' establishment), 
#'   to be retained. If \code{FALSE} (the default), all such dubious points will
#'   be excluded. Ignored if \code{climate_suitability_path} is provided. Note 
#'   that manual checking is not possible when using \code{\link{excel_to_plan}}
#'   since the required interactivity will interrupt plan processing.
#' @param total_tourists Numeric. The total number of tourists entering
#'   Australia.
#' @param prob_tourists Numeric. The per capita rate of pest entry that 
#'   applies to tourists.
#' @param total_returning Numeric. The total number of returnig residents 
#'   entering Australia.
#' @param prob_returning Numeric. The per capita rate of pest entry that 
#'   applies to returning residents.
#' @param total_torres Numeric. The total number of passenges entering 
#'   Australia via the Torres Strait Islands pathway.
#' @param prob_torres Numeric. The per capita rate of pest entry that 
#'   applies to passengers entering via the Torres Strait Islands
#'   pathway.
#' @param total_mail Numeric. The total volume of mail entering Australia.
#' @param prob_mail The rate of pest entry per unit volume of mail.
#' @param total_vessels Numeric. The total volume of marine vessels entering
#'   Australia.
#' @param prob_vessels The rate of pest entry per vessel.
#' @param total_fertiliser Numeric. The total volume of fertiliser entering
#'   Australia.
#' @param prob_fertiliser The rate of pest entry per unit volume of fertiliser.
#' @param total_machinery Numeric. The total volume of machinery entering
#'   Australia.
#' @param prob_machinery The rate of pest entry per unit volume of machinery.
#' @param prob_containers The rate of pest entry per container. 
#' @param total_nurserystock Numeric. The total volume of nursery stock 
#' entering Australia.
#' @param prob_nurserystock The rate of pest entry per unit volume of nursery
#'   stock.
#' @param total_goods Numeric. The total volume of goods entering Australia.
#' @param prob_goods The rate of pest entry per unit volume of goods.
#' @details To simplify reproducibility, \code{edmaps} provides an 
#'   \emph{Excel} interface for specifying species parameters relevant to
#'   estimating establishment likelihood. An example spreadsheet is 
#'   bundled with the package, available at the path given by 
#'   \code{system.file('extdata/parameters.xlsx', package='edmaps')}. The 
#'   spreadsheet has two sheets, the first specifying "global" parameters
#'   that will apply to all species (e.g. file paths to rasters that will),
#'   be used regardless of species identity and the second specifying
#'   parameters that can vary by species. In the second sheet, each row
#'   corresponds to a separate species. Tooltips and data validation
#'   guide the user with respect to expected/allowable data.
#' @seealso \code{\link{excel_to_plan}}
#' @importFrom glue glue
#' @importFrom CoordinateCleaner clean_coordinates
#' @importFrom raster extent raster res compareCRS
#' @importFrom sp CRS
#' @importFrom sf st_bbox
#' @importFrom readr read_csv
#' @importFrom dplyr select bind_rows
#' @importFrom drake file_in file_out code_to_plan
#' @importFrom gdalUtilities gdal_translate
#' @importFrom magrittr '%>%'
#' @export
species_plan <- function(species, clum_classes, nvis_classes, pathways,
  include_abiotic_weight=TRUE, climate_suitability_path,
  exclude_bioclim_vars=NULL, include_ndvi=TRUE, aggregated_res=c(5000, 5000),
  make_interactive_maps=TRUE, clum_path,  nvis_path,  ndvi_path,
  airport_beta=log(0.5)/200, airport_tsi_beta=log(0.5)/10, port_data_path,
  port_weight_beta, fertiliser_data_path, nrm_path, containers_data_path,
  postcode_path, occurrence_path, infected_countries, cabi_path, use_gbif=FALSE, 
  gbif_species, gbif_min_year=1970, gbif_max_uncertainty=20000, gbif_username,
  gbif_password, manual_check_flagged_records=FALSE, total_tourists, 
  prob_tourists, total_returning, prob_returning, total_torres, prob_torres, 
  total_mail, prob_mail, total_vessels, prob_vessels, total_fertiliser, 
  prob_fertiliser, total_machinery, prob_machinery, prob_containers, 
  total_nurserystock, prob_nurserystock, total_goods, prob_goods) {
  
  # prepare extent and resolution
  res <- c(1000, 1000) # enforce 1km for now - memory safe
  extent <- c(-1888000, 2122000, -4847000, -1010000) 
  # ^ hard-code national extent 
  if(!is.numeric(aggregated_res)) 
    stop('res must be a numeric vector with 1 or 2 elements.')
  if(length(aggregated_res) == 1) {
    aggregated_res <- c(aggregated_res, aggregated_res)
  }
  if(length(aggregated_res) > 2) stop('Supplied aggregated_res invalid.')
  if(aggregated_res[1] != aggregated_res[2]) 
    stop('Horizontal and vertical aggregated resolutions must be equal.')
  if(aggregated_res[1] < res[1]) stop('aggregated_res cannot be less than res.')
  if(aggregated_res[1] %% res[1] != 0) {
    stop('aggregated_res must be a multiple of res.')
  }
  agg_factor <- aggregated_res[1]/res[1]
  agg_factor_aux <- 5 # agg factor for aggregated outputs that are not EL maps
  aggregated_res_aux <- agg_factor_aux*res
  
  # remove leading/trailing white space from species name, and replace all
  # internal white space with underscores. Required, since species name
  # is used to name targets
  species <- gsub('\\s+', '_', gsub('^\\s+|\\s+$', '', species))
  
  pathways <- match.arg(
    pathways, c('containers', 'fertiliser', 'goods', 'machinery', 'mail', 
                'nurserystock', 'residents', 'torres', 'tourists', 'vessels'), 
    several.ok=TRUE
  )
 
  if(!missing(infected_countries) && !missing(cabi_path)) {
    stop('Only one of cabi_path or infected_countries should be provided.')
  }
   
  if(missing(clum_classes) && missing(nvis_classes))
    stop('Provide clum_classes and/or nvis_classes to define ',
         'distribution of host plants.')
  
  if(!missing(climate_suitability_path)) {
    r <- raster::raster(climate_suitability_path)
    if(any(as.vector(raster::extent(r)) != extent)) {
      stop(species, ': exent of climate suitability raster must be', 
           paste0(c("xmin: ", "xmax: ", "ymin: ", "ymax: "),
                  extent, collapse=", "))
    }
    if(!identical(raster::res(r), c(1000, 1000)))
      stop(species, ": resolution of climate_suitability raster must be ",
           "1000 x 1000 m.")
    if(!raster::compareCRS(r, sp::CRS("+init=epsg:3577")))
      stop(species, ": resolution of climate_suitability raster must be ",
           "Australian Albers (EPSG:3577).")
    rm(r)
  }
  
  # hardcode some paths to processed data (some of these won't exist until 
  # they're created by the plan)
  airport_weight_path <- sprintf(
    "risk_layers/pathway/processed/airport_dist_weight_%s.tif", res[1])
  cairns_airport_weight_path <- sprintf(
    "risk_layers/pathway/processed/cairns_airport_dist_weight_%s.tif", res[1])
  ndvi_norm_path <- sprintf(
    "risk_layers/biotic/processed/NDVI_norm_%s.tif", res[1])
  tourist_beds_path <- sprintf(
    "risk_layers/pathway/processed/tourism_beds_%s.tif", res[1])
  climate_path <- "risk_layers/abiotic/bioclim_10m"
  fert_weight_path <- sprintf(
    "outputs/not_pest_specific/fert_weight_%s.tif", res[1])
  pop_density_path <- 
    "risk_layers/pathway/raw_data/Population/pop_density_1000m.tif"
  clum_rle_path <- sprintf(
    'risk_layers/biotic/processed/clum_rle_%s.rds', res[1])
  nvis_rle_path <- sprintf(
    'risk_layers/biotic/processed/nvis_rle_%s.rds', res[1])
    
  
  file.create(f <- tempfile())
  
  if(!missing(clum_classes)) {
    cat(glue::glue(
      'clum_rast <- binarize_and_aggregate(
      rle = drake::file_in("{clum_rle_path}"),
      outfile = 
        drake::file_out("outputs/{species}/auxiliary/{species}_clum_raster_{res[1]}.tif"),
      categories = {paste(deparse(clum_classes), collapse="")},
      res = {deparse(res)},
      extent = {deparse(extent)},
      overwrite = TRUE)
    \n\n'), file=f, append=TRUE)
  }
  
  if(!missing(nvis_classes)) {
    cat(glue::glue('
      nvis_rast <- binarize_and_aggregate(
        rle = drake::file_in("{nvis_rle_path}"),
        outfile = drake::file_out(
          "outputs/{species}/auxiliary/{species}_nvis_raster_{res[1]}.tif"
        ),
        categories = {paste(deparse(nvis_classes), collapse="")},
        res = {deparse(res)},
        extent = {deparse(extent)},
        overwrite = TRUE)
    \n\n'), file=f, append=TRUE)
  }
  
  if(!missing(clum_classes) && !missing(nvis_classes)) {
    cat(glue::glue('
      host_rast <- suitability(list(
        drake::file_in("outputs/{species}/auxiliary/{species}_clum_raster_{res[1]}.tif"),
        drake::file_in("outputs/{species}/auxiliary/{species}_nvis_raster_{res[1]}.tif")
        ), outfile=drake::file_out(
          "outputs/{species}/auxiliary/{species}_host_raster_{res[1]}.tif"
        )
      )
    \n\n'), file=f, append=TRUE)
  } else if(!missing(clum_classes)) {
    cat(glue::glue('
      host_rast <- 
        file.copy(
          drake::file_in(
            "outputs/{species}/auxiliary/{species}_clum_raster_{res[1]}.tif"
          ),
          drake::file_out(
            "outputs/{species}/auxiliary/{species}_host_raster_{res[1]}.tif"
          )
        )
    \n\n'), file=f, append=TRUE)
  } else if(!missing(nvis_classes)) {
    cat(glue::glue('
      host_rast <- 
        file.copy(
          drake::file_in(
            "outputs/{species}/auxiliary/{species}_nvis_raster_{res[1]}.tif"
          ), 
          drake::file_out(
            "outputs/{species}/auxiliary/{species}_host_raster_{res[1]}.tif"
          )
        )
    \n\n'), file=f, append=TRUE)
  }
  
  if('tourists' %in% pathways) {
    cat(glue::glue('
      tourist_arrivals <- arrivals_by_tourists(
        tourist_beds = drake::file_in("{tourist_beds_path}"),
        airport_weight = drake::file_in("{airport_weight_path}"),
        total_tourists = {total_tourists},
        probability = {paste(deparse(prob_tourists), collapse="")},
        outfile = drake::file_out(
          "outputs/{species}/auxiliary/{species}_tourists_arrivals_{res[1]}.tif"
        )
      )
    \n\n'), file=f, append=TRUE)
  }
  
  if('residents' %in% pathways) {
    cat(glue::glue('
      residents_arrivals <- arrivals_by_residents(
        pop_density = drake::file_in("{pop_density_path}"),
        total_returning = {total_returning},
        probability = {paste(deparse(prob_returning), collapse="")},
        outfile = drake::file_out(
         "outputs/{species}/auxiliary/{species}_residents_arrivals_{res[1]}.tif"
        )
      )
    \n\n'), file=f, append=TRUE)
  }
  
  if('torres' %in% pathways) {
    cat(glue::glue('
      torres_arrivals <- arrivals_by_torres(
        pop_density = drake::file_in("{pop_density_path}"),
        airport_weight = drake::file_in("{cairns_airport_weight_path}"),
        total_passengers = {total_torres},
        probability = {paste(deparse(prob_torres), collapse="")},
        outfile = drake::file_out(
          "outputs/{species}/auxiliary/{species}_torres_arrivals_{res[1]}.tif"
        )
      )
    \n\n'), file=f, append=TRUE)
  }
  
  if('mail' %in% pathways) {
    cat(glue::glue('
      mail_arrivals <- arrivals_by_mail(
        pop_density = drake::file_in("{pop_density_path}"),
        total_mail = {total_mail},
        probability = {paste(deparse(prob_mail), collapse="")},
        outfile = drake::file_out(
          "outputs/{species}/auxiliary/{species}_mail_arrivals_{res[1]}.tif"
        )
      )
    \n\n'), file=f, append=TRUE)
  }
  
  if('vessels' %in% pathways) {
    cat(glue::glue('
      marine_port_weights =
        port_weights(
          template_raster = drake::file_in(
            "risk_layers/auxiliary/aus_mask_clum_{res[1]}.tif"
          ), 
        port_data = drake::file_in("{port_data_path}"),
        beta = {port_weight_beta},
        outfile = drake::file_out(
          "outputs/{species}/auxiliary/{species}_port_weights_{res[1]}.tif"
        )
      )
    
      plot_port_weight = 
        static_map(
          ras = drake::file_in(
            "outputs/{species}/auxiliary/{species}_port_weights_{res[1]}.tif"
          ),
          xlim = c(112.76, 155),
          ylim = c(-44.03, -9.21),
          legend_title = 
            "log10(Port weight [{round(aggregated_res_aux[1]/1000, 2)}km])",
          set_value_range = c(0, Inf),
          transparency = 1, 
          scale_type = "log10",
          aggregate_raster = list({agg_factor_aux}, max), 
          height = 6.5,
          outfile = drake::file_out(
            "outputs/{species}/auxiliary/{species}_port_weights_{aggregated_res_aux[1]}.pdf"
          )
        )
    
      vessels_arrivals <- arrivals_by_vessels(
        port_weight = drake::file_in(
          "outputs/{species}/auxiliary/{species}_port_weights_{res[1]}.tif"
        ),
        n_vessels = {total_vessels},
        probability = {paste(deparse(prob_vessels), collapse="")},
        outfile = drake::file_out(
          "outputs/{species}/auxiliary/{species}_vessels_arrivals_{res[1]}.tif"
        )
      )
    \n\n'), file=f, append=TRUE)
  }
  
  if('fertiliser' %in% pathways) {
    cat(glue::glue('
      fert_arrivals <- arrivals_by_fertiliser(
        fertiliser_weight = drake::file_in("{fert_weight_path}"),
        fertiliser_units = {total_fertiliser},
        probability = {paste(deparse(prob_fertiliser), collapse="")},
        outfile = drake::file_out(
          "outputs/{species}/auxiliary/{species}_fertiliser_arrivals_{res[1]}.tif"
        )
      )
    \n\n'), file=f, append=TRUE)
  }
  
  if('machinery' %in% pathways) {
    cat(glue::glue('
      machinery_arrivals <- arrivals_by_machinery(
        pop_density = drake::file_in("{pop_density_path}"),
        total_machinery = {total_machinery},
        probability = {paste(deparse(prob_machinery), collapse="")},
        outfile = drake::file_out(
          "outputs/{species}/auxiliary/{species}_machinery_arrivals_{res[1]}.tif"
        )
      )
    \n\n'), file=f, append=TRUE)
  }
  
  if('containers' %in% pathways) {
    cat(glue::glue('
      container_arrivals <- arrivals_by_containers(
        container_weights = container_weight,
        port_data = drake::file_in("{port_data_path}"),
        template_raster = drake::file_in(
          "outputs/{species}/auxiliary/{species}_host_raster_{res[1]}.tif"
        ),
        probability = {paste(deparse(prob_containers), collapse="")},
        outfile = drake::file_out(
          "outputs/{species}/auxiliary/{species}_containers_arrivals_{res[1]}.tif"
        )
      )
    \n\n'), file=f, append=TRUE)
  }
  
  if('goods' %in% pathways) {
    cat(glue::glue('
      goods_arrivals <- arrivals_by_goods(
        pop_density = drake::file_in("{pop_density_path}"),
        total_imports = {total_goods},
        probability = {paste(deparse(prob_goods), collapse="")},
        outfile = drake::file_out(
          "outputs/{species}/auxiliary/{species}_goods_arrivals_{res[1]}.tif"
        )
      )
    \n\n'), file=f, append=TRUE)
  }
  
  if('nurserystock' %in% pathways) {
    cat(glue::glue('
      nurserystock_arrivals <- arrivals_by_nurserystock(
        pop_density = drake::file_in("{pop_density_path}"),
        total_imports = {total_nurserystock},
        probability = {paste(deparse(prob_nurserystock), collapse="")},
        outfile = drake::file_out(
          "outputs/{species}/auxiliary/{species}_nurserystock_arrivals_{res[1]}.tif"
        )
      )
    \n\n'), file=f, append=TRUE)
  }
  
  cat('total_arrivals <- combine_arrivals(summarise_uncertainty=TRUE, x=c(\n', 
      paste(sprintf(
        'drake::file_in("outputs/%1$s/auxiliary/%1$s_%2$s_arrivals_%3$s.tif")', 
        species, pathways, res[1]), 
        collapse=',\n '
      ),
      sprintf(
        '\n), outfile=drake::file_out("outputs/%1$s/%1$s_total_arrivals_%2$s.tif"))\n\n', 
        species, res[1]
      ),
      file=f, append=TRUE)
  
  # If climate suitability path is missing but include_biotic_weight is TRUE,
  # do range-bagging with GBIF and/or expert occurrence data.
  if(missing(climate_suitability_path) && isTRUE(include_abiotic_weight)) {
    
    if(isTRUE(use_gbif)) {
      cat(glue::glue('
        country_reference <- 
          sf::as_Spatial(
            sf::st_buffer(
              rnaturalearth::ne_countries(scale=50, returnclass="sf"), 0
            ) # fix self-intersection
          )
      \n\n'), file=f, append=TRUE)
      
      if(!missing(gbif_username) && !missing(gbif_password) &&
         nchar(sub('^\\s+$', '', gbif_username)) > 0 && 
         nchar(sub('^\\s+$', '', gbif_password)) > 0) {
        cat(glue::glue('
        gbif_records <- get_gbif_records(
          taxon=<<paste0(deparse(c(glue::glue("{gbif_species}"))), collapse="")>>,
          min_year=<<gbif_min_year>>, coord_uncertainty=<<gbif_max_uncertainty>>,
          username="<<gbif_username>>", email="cebra.apps@gmail.com", 
          pwd="<<gbif_password>>", method="download")
      \n\n', .open='<<', .close='>>'), file=f, append=TRUE)
      } else {
        cat(glue::glue('
        gbif_records <- get_gbif_records(
          taxon=<<paste0(deparse(c(glue::glue("{gbif_species}"))), collapse="")>>,
          min_year=<<gbif_min_year>>, coord_uncertainty=<<gbif_max_uncertainty>>)
      \n\n', .open='<<', .close='>>'), file=f, append=TRUE)
      }
      
      
      cat(glue::glue('
        clean_gbif <- dplyr::select(
        # ^ target is renamed before returning from species_plan
          CoordinateCleaner::clean_coordinates(
            x = dplyr::mutate({species}_gbif_records, species="{species}"),
            lon = "decimalLongitude",
            lat = "decimalLatitude",
            countries = "countryCode",
            species = "species",
            country_ref = country_reference, 
            country_refcol = "iso_a3",
            tests = c("capitals",
                      "centroids", 
                      "countries",
                      "equal", 
                      "gbif", 
                      "institutions",
                      "duplicates",
                      "zeros"),
            capitals_rad = 5000,
            centroids_rad = 10000,
            inst_rad = 100,
            zeros_rad = 0.5,
            value = "clean"),
          Longitude = decimalLongitude, Latitude = decimalLatitude)
      \n\n'), file=f, append=TRUE)
    }
    
    if(!missing(occurrence_path)) {
      cat(glue::glue('
        clean_expert_records <- dplyr::select(
          CoordinateCleaner::clean_coordinates(
            x = readr::read_csv(drake::file_in("{occurrence_path}")), 
            lon = "Longitude", 
            lat = "Latitude",
            species = "Species",
            tests = c("capitals",
                      "centroids", 
                      "equal", 
                      "gbif", 
                      "institutions",
                      "duplicates",
                      "zeros"),
            capitals_rad = 5000,
            centroids_rad = 10000,
            inst_rad = 100,
            zeros_rad = 0.5,
            value = "clean"),
          Longitude, Latitude)
       \n\n'), file=f, append=TRUE)
    }
    
    if(!missing(occurrence_path) && isTRUE(use_gbif)) {
      cat(glue::glue('
        all_records <- unique(
          dplyr::bind_rows({species}_clean_gbif, {species}_clean_expert_records)
        )
      \n\n'), file=f, append=TRUE)
    } else if(!missing(occurrence_path)) {
      cat(glue::glue('
        all_records <- unique({species}_clean_expert_records)
      \n\n'), file=f, append=TRUE)
    } else if(isTRUE(use_gbif)) {
      cat(glue::glue('
        all_records <- unique({species}_clean_gbif)
      \n\n'), file=f, append=TRUE)
    }
    
    if(!missing(cabi_path)) {
      if(!include_abiotic_weight) {
        warning('{species}: ',
                'CABI paths and infected country list ignored when ',
                'abiotic_weight is FALSE.')
      }
      cat(glue::glue('
        records <- record_flagger(occurrence_records = {species}_all_records,
          manual_check = {manual_check_flagged_records},
          return_df = FALSE,
          cabi_ref = drake::file_in("{cabi_path}"))
      \n\n'), file=f, append=TRUE) 
    }
    
    if(!missing(infected_countries)) {
      if(!include_abiotic_weight) {
        warning('{species}: ',
                'CABI paths and infected country list ignored when ',
                'abiotic_weight is FALSE.')
      }
      cat(glue::glue('
        records <- record_flagger(occurrence_records = <<species>>_all_records,
          manual_check = <<manual_check_flagged_records>>,
          return_df = FALSE,
          infected_countries = <<paste0(deparse(c(glue::glue("{infected_countries}"))), collapse="")>>)
      \n\n', .open='<<', .close='>>'), file=f, append=TRUE)
    }
    
    do_flag <- !missing(infected_countries) | !missing(cabi_path)
    
    if(do_flag) {
      cat(glue::glue('
        rangebag <- range_bag(
          occurrence_data = {species}_records,
          bioclim_dir = drake::file_in("{climate_path}"),
          n_dims = 2,
          n_models = 100,
          p = 0.5,
          exclude_vars = {deparse(exclude_bioclim_vars)},
          outfile = drake::file_out(
            "outputs/{species}/auxiliary/{species}_global_climsuit_10min.tif"
          )
        )
      \n\n'), file=f, append=TRUE)
      
      cat(glue::glue('
        plot_global_climsuit <- plot_raster(
          object = drake::file_in(
            "outputs/{species}/auxiliary/{species}_global_climsuit_10min.tif"
          ),
          legend_title = "Suitability (10\')",
          occurrence_data = {species}_records,
          pt_col = "blue",
          height = 4.5,
          outfile = drake::file_out(
          "outputs/{species}/static_maps/{species}_world_climsuit_10min.pdf")
        ) 
      \n\n'), file=f, append=TRUE)
    } else {
      cat(glue::glue('
        rangebag <- range_bag(
          occurrence_data = {species}_all_records,
          bioclim_dir = drake::file_in("{climate_path}"),
          n_dims = 2,
          n_models = 100,
          p = 0.5,
          exclude_vars = {deparse(exclude_bioclim_vars)},
          outfile = drake::file_out(
            "outputs/{species}/auxiliary/{species}_global_climsuit_10min.tif"
          )
        )
      \n\n'), file=f, append=TRUE)
      
      cat(glue::glue('
        plot_global_climsuit <- plot_raster(
          object = drake::file_in(
            "outputs/{species}/auxiliary/{species}_global_climsuit_10min.tif"
          ),
          legend_title = "Suitability (10\')",
          occurrence_data = {species}_all_records,
          pt_col = "blue",
          height = 4.5,
          outfile = drake::file_out(
          "outputs/{species}/static_maps/{species}_world_climsuit_10min.pdf")
        ) 
      \n\n'), file=f, append=TRUE)
    }
    
    
    cat(glue::glue('
      climsuit <- gdal_reproject(
        infile = drake::file_in(
          "outputs/{species}/auxiliary/{species}_global_climsuit_10min.tif"
        ),
        outfile = drake::file_out(
          "outputs/{species}/auxiliary/{species}_aust_climsuit_{res[1]}.tif"
        ),
        res = {deparse(res)},
        tgt_proj = "EPSG:3577",
        tgt_extent = {deparse(extent[c(1, 3, 2, 4)])}
      )
    \n\n'), file=f, append=TRUE)
    
    
    # If climate_suitability_path is provided (regardless of setting of 
    # include_abiotic_weight), just copy it to appropriate path.
    # Use gdal_translate in case it's not a tiff to begin with.
  } else if(!missing(climate_suitability_path)) { 
    cat(glue::glue('
      climsuit <- gdalUtilities::gdal_translate(
        drake::file_in("{climate_suitability_path}"),
        drake::file_out("outputs/{species}/auxiliary/{species}_aust_climsuit_{res[1]}.tif")
      )
    \n\n'), file=f, append=TRUE)
  }
  
  
  if(isTRUE(include_ndvi)) {
    cat(glue::glue('
      biotic_suitability <- suitability(
        x = list(
          drake::file_in("{ndvi_norm_path}"), 
          drake::file_in(
            "outputs/{species}/auxiliary/{species}_host_raster_{res[1]}.tif"
          )
        ),
        outfile = drake::file_out(
          "outputs/{species}/{species}_biotic_suitability_{res[1]}.tif"
        )
      )
    \n\n'), file=f, append=TRUE)  
  } else {
    cat(glue::glue('
      biotic_suitability <- file.copy(
        drake::file_in("outputs/{species}/auxiliary/{species}_host_raster_{res[1]}.tif"),
        drake::file_out(
          "outputs/{species}/{species}_biotic_suitability_{res[1]}.tif"
        )
      )
    \n\n'), file=f, append=TRUE)
  }
  
  cat(glue::glue('
    plot_biotic_suitability <- static_map(
      ras = drake::file_in(
        "outputs/{species}/{species}_biotic_suitability_{res[1]}.tif"
      ),
      xlim = c(112.76, 155),
      ylim = c(-44.03, -9.21),
      legend_title = "Suitability ({round(aggregated_res_aux[1]/1000, 2)}km)",
      transparency = 1, 
      aggregate_raster = list({agg_factor_aux}, max), 
      set_value_range = c(0, Inf),
      height = 7,
      outfile = drake::file_out(
        "outputs/{species}/static_maps/{species}_biotic_suitability_{aggregated_res_aux[1]}.pdf"
      )
    )
  \n\n'), file=f, append=TRUE)
  
  if(isTRUE(include_abiotic_weight)) {
    
    cat(glue::glue('
      plot_climsuit <- static_map(
        ras = drake::file_in(
          "outputs/{species}/auxiliary/{species}_aust_climsuit_{res[1]}.tif"
        ),
        xlim = c(112.76, 155),
        ylim = c(-44.03, -9.21),
        legend_title = "Suitability ({round(aggregated_res_aux[1]/1000, 2)}km)",
        transparency = 1, 
        aggregate_raster = list({agg_factor_aux}, max), 
        height = 7,
        outfile = drake::file_out(
          "outputs/{species}/static_maps/{species}_climsuit_{aggregated_res_aux[1]}.pdf"
        )
      )  
    \n\n'), file=f, append=TRUE)
    
    cat(glue::glue('
    suitability <- suitability(
        x = list(
          drake::file_in(
            "outputs/{species}/auxiliary/{species}_aust_climsuit_{res[1]}.tif"
          ),
          drake::file_in(
            "outputs/{species}/{species}_biotic_suitability_{res[1]}.tif"
          )
        ),
        outfile = drake::file_out(
          "outputs/{species}/{species}_suitability_{res[1]}.tif"
        )
      )
    \n\n'), file=f, append=TRUE) 
  } else {
    cat(glue::glue('
      suitability <- file.copy(
        drake::file_in(
          "outputs/{species}/{species}_biotic_suitability_{res[1]}.tif"
        ),
        drake::file_out("outputs/{species}/{species}_suitability_{res[1]}.tif")
      )
    \n\n'), file=f, append=TRUE)
  }
  
  cat(glue::glue('
    plot_suitability <- static_map(
      ras = drake::file_in(
        "outputs/{species}/{species}_suitability_{res[1]}.tif"
      ),
      xlim = c(112.76, 155),
      ylim = c(-44.03, -9.21),
      legend_title = "Suitability ({round(aggregated_res_aux[1]/1000, 2)}km)",
      transparency = 1, 
      aggregate_raster = list({agg_factor_aux}, max), 
      set_value_range = c(0, Inf),
      height = 7,
      outfile = drake::file_out(
        "outputs/{species}/static_maps/{species}_suitability_{aggregated_res_aux[1]}.pdf"
      )
    )
  \n\n'), file=f, append=TRUE)      
  
  
  cat(glue::glue('
    establishment_likelihood <- establishment_likelihood(
      total_arrivals = drake::file_in(
        "outputs/{species}/{species}_total_arrivals_{res[1]}.tif"
      ),
      suitability = drake::file_in(
        "outputs/{species}/{species}_suitability_{res[1]}.tif"
      ),
      outfile = drake::file_out(
        "outputs/{species}/{species}_edmap_{res[1]}.tif")
      )
  \n\n'), file=f, append=TRUE)      
  
  
  cat(glue::glue('
    establishment_likelihood_agg <- aggregate_raster(
      rast = drake::file_in("outputs/{species}/{species}_edmap_{res[1]}.tif"),
      outfile = drake::file_out(
        "outputs/{species}/{species}_edmap_{aggregated_res[1]}.tif"
      ),
      aggregate_factor = {agg_factor},
      fun = sum)
  \n\n'), file=f, append=TRUE)      
  
  
  cat(glue::glue('
    cumu_establishment_likelihood_agg <- captured_by_ncells(
      infiles = drake::file_in(
        "outputs/{species}/{species}_edmap_{aggregated_res[1]}.tif"
      ),
      names = "Cumulative Establishment Likelihood ({round(aggregated_res[1]/1000, 2)}km)",
      n_cells = 2000
    )
  \n\n'), file=f, append=TRUE)      
  
  if(isTRUE(make_interactive_maps)) {
    cat(glue::glue('
      edmap <- interactive_map(
        ras = drake::file_in(
          "outputs/{species}/{species}_edmap_{res[1]}.tif"
        ),
        layer_name = "log10(Establishment likelihood {round(res[1]/1000, 2)}km)",
        set_value_range = c(10^-8, Inf),
        scale_type = "log10",
        outfile = drake::file_out(
          "outputs/{species}/interactive_maps/{species}_edmap_{res[1]}.html"
        )
      )
    \n\n'), file=f, append=TRUE)      
    
    cat(glue::glue('
      arrivals_map <- interactive_map(
        ras = drake::file_in(
          "outputs/{species}/{species}_total_arrivals_{res[1]}.tif"
        ),
        layer_name = "log10(Arrivals {round(res[1]/1000, 2)}km)",
        set_value_range = c(10^-8, Inf),
        scale_type = "log10",
        outfile = drake::file_out(
          "outputs/{species}/interactive_maps/{species}_arrival_map_{res[1]}.html"
        )
      )
    \n\n'), file=f, append=TRUE)      
    
    cat(glue::glue('
      suitability_map <- interactive_map(
        ras = drake::file_in(
          "outputs/{species}/{species}_suitability_{res[1]}.tif"
        ),
        layer_name = "Environmental suitability {round(res[1]/1000, 2)}km",
        set_value_range = c(0, Inf),
        outfile = drake::file_out(
          "outputs/{species}/interactive_maps/{species}_suitability_map_{res[1]}.html"
        )
      )
    \n\n'), file=f, append=TRUE)
  }
  
  cat(glue::glue('
    plot_national_establishment_likelihood <- static_map(
      ras = drake::file_in("outputs/{species}/{species}_edmap_{res[1]}.tif"),
      xlim = c(112.76, 155),
      ylim = c(-44.03, -9.21),
      legend_title = "log10(EL {round(aggregated_res[1]/1000, 2)}km)",
      set_value_range = c(10^-8, Inf),
      scale_type = "log10",
      transparency = 1, 
      aggregate_raster = list({agg_factor}, max), 
      height = 7,
      outfile = drake::file_out(
        "outputs/{species}/static_maps/{species}_edmap_national_{aggregated_res[1]}.pdf"
      )
    )
  \n\n'), file=f, append=TRUE)
  
  cat(glue::glue('
    cairns_edmap <- static_map(
      ras = drake::file_in("outputs/{species}/{species}_edmap_{res[1]}.tif"),
      xlim = c(145.23, 146.14),
      ylim = c(-17.34, -16.68),
      legend_title = "log10(EL {round(res[1]/1000, 2)}km)",
      set_value_range = c(10^-8, Inf),
      scale_type = "log10", 
      transparency = 0.7,
      colramp_entire_range = TRUE,
      height = 7,
      outfile = drake::file_out(
        "outputs/{species}/static_maps/{species}_edmap_cairns_{res[1]}.pdf"
      )
    )
  \n\n'), file=f, append=TRUE)
  
  cat(glue::glue('
    brisbane_edmap <- static_map(
      ras = drake::file_in(
        "outputs/{species}/{species}_edmap_{res[1]}.tif"
      ),
      xlim = c(151.76, 153.86),
      ylim = c(-28.11, -26.45),
      legend_title = "log10(EL {round(res[1]/1000, 2)}km)",
      set_value_range = c(10^-8, Inf),
      scale_type = "log10", 
      transparency = 0.7,
      colramp_entire_range = TRUE,
      height = 7,
      outfile = drake::file_out(
        "outputs/{species}/static_maps/{species}_edmap_brisbane_{res[1]}.pdf"
      )
    )
  \n\n'), file=f, append=TRUE)
  
  cat(glue::glue('
    sydney_edmap <- static_map(
      ras = drake::file_in(
        "outputs/{species}/{species}_edmap_{res[1]}.tif"
      ),
      xlim = c(150.29, 151.5),
      ylim = c(-34.25, -33.51),
      legend_title = "log10(EL {round(res[1]/1000, 2)}km)",
      set_value_range = c(10^-8, Inf),
      scale_type = "log10", 
      transparency = 0.7,
      colramp_entire_range = TRUE,
      height = 7,
      outfile = drake::file_out(
        "outputs/{species}/static_maps/{species}_edmap_sydney_{res[1]}.pdf"
      )
    )
  \n\n'), file=f, append=TRUE)
  
  cat(glue::glue('
    melbourne_edmap <- static_map(
      ras = drake::file_in(
        "outputs/{species}/{species}_edmap_{res[1]}.tif"
      ),
      xlim = c(144.5, 145.5),
      ylim = c(-38.1, -37.5),
      legend_title = "log10(EL {round(res[1]/1000, 2)}km)",
      set_value_range = c(10^-8, Inf),
      scale_type = "log10", 
      transparency = 0.7,
      colramp_entire_range = TRUE,
      height = 7,
      outfile = drake::file_out(
        "outputs/{species}/static_maps/{species}_edmap_melbourne_{res[1]}.pdf"
      )
    )
  \n\n'), file=f, append=TRUE)
  
  cat(glue::glue('
    hobart_edmap <- static_map(
      ras = drake::file_in(
        "outputs/{species}/{species}_edmap_{res[1]}.tif"
      ),
      xlim = c(146.4, 148.13),
      ylim = c(-43.34, -42.33),
      legend_title = "log10(EL {round(res[1]/1000, 2)}km)",
      set_value_range = c(10^-8, Inf),
      scale_type = "log10", 
      transparency = 0.7,
      colramp_entire_range = TRUE,
      height = 7,
      outfile = drake::file_out(
        "outputs/{species}/static_maps/{species}_edmap_hobart_{res[1]}.pdf"
      )
    )
  \n\n'), file=f, append=TRUE)
  
  cat(glue::glue('
    adelaide_edmap <- static_map(
      ras = drake::file_in(
        "outputs/{species}/{species}_edmap_{res[1]}.tif"
      ),
      xlim = c(138, 139.5),
      ylim = c(-36, -34),
      legend_title = "log10(EL {round(res[1]/1000, 2)}km)",
      set_value_range = c(10^-8, Inf),
      scale_type = "log10", 
      transparency = 0.7,
      colramp_entire_range = TRUE,
      height = 7,
      outfile = drake::file_out(
        "outputs/{species}/static_maps/{species}_edmap_adelaide_{res[1]}.pdf"
      )
    )
  \n\n'), file=f, append=TRUE)
  
  cat(glue::glue('
    perth_edmap <- static_map(
      ras = drake::file_in(
        "outputs/{species}/{species}_edmap_{res[1]}.tif"
      ),
      xlim = c(115.28, 116.86),
      ylim = c(-32.56, -31.41),
      legend_title = "log10(EL {round(res[1]/1000, 2)}km)",
      set_value_range = c(10^-8, Inf),
      scale_type = "log10", 
      transparency = 0.7,
      colramp_entire_range = TRUE,
      height = 7,
      outfile = drake::file_out(
        "outputs/{species}/static_maps/{species}_edmap_perth_{res[1]}.pdf"
      )
    )
  \n\n'), file=f, append=TRUE)
  
  cat(glue::glue('
    darwin_edmap <- static_map(
      ras = drake::file_in(
        "outputs/{species}/{species}_edmap_{res[1]}.tif"
      ),
      xlim = c(130.5, 131.65),
      ylim = c(-12.88, -12.09),
      legend_title = "log10(EL {round(res[1]/1000, 2)}km)",
      set_value_range = c(10^-8, Inf),
      scale_type = "log10", 
      transparency = 0.7,
      colramp_entire_range = TRUE,
      height = 7,
      outfile = drake::file_out(
        "outputs/{species}/static_maps/{species}_edmap_darwin_{res[1]}.pdf"
      )
    )
  \n\n'), file=f, append=TRUE)
  
  plan <- drake::code_to_plan(f)
  plan$target <- ifelse(plan$target=='country_reference', 'country_reference',
                        paste0(species, '_', plan$target))
  rbind(plan_globals(
    clum_path=clum_path,
    nvis_path=nvis_path,
    ndvi_path=ndvi_path,
    fertiliser_data_path=fertiliser_data_path, 
    nrm_path=nrm_path, 
    containers_data_path=containers_data_path,
    postcode_path=postcode_path,
    airport_beta=airport_beta,
    airport_tsi_beta=airport_tsi_beta), plan)
}
