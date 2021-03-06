#' Create a drake plan describing targets relating to global parameters
#'
#' Create a drake plan that describes targets relating to global parameters 
#' to be used by individual species plans.
#' @param clum_path Character. File path to \href{http://www.agriculture.gov.au/abares/aclump/Pages/land-use/Catchment-scale-land-use-of-Australia-2018.aspx}{Catchment Scale Land Use of Australia (ACLUM)} raster.
#' @param nvis_path Character. File path to the \href{https://www.environment.gov.au/land/native-vegetation/national-vegetation-information-system/data-products}{National Vegetation Information System (NVIS)} raster dataset.
#' @param ndvi_path Character. File path to the \href{http://www.bom.gov.au/jsp/awap/ndvi/index.jsp?colour=colour&time=history/nat/2018110120190430&step=7&map=ndviave&period=6month&area=nat}{Normalised Difference Vegetation Index (NDVI)} raster dataset.
#' @param fertiliser_data_path Character. File path to a csv containing data 
#'   describing fertiliser use, available \href{https://www.abs.gov.au/AUSSTATS/abs@@.nsf/DetailsPage/4627.02016-17}{here}.
#' @param nrm_path Character. File path to shapefile of \href{https://www.abs.gov.au/AUSSTATS/abs@@.nsf/DetailsPage/1270.0.55.003July 2016}{Natural Resource Management Regions}.
#' @param containers_data_path Character. File path to xlsx file containing 
#'   data about shipping container movements, available \href{https://www.abs.gov.au/AUSSTATS/abs@@.nsf/DetailsPage/5368.0.55.0182009-10}{here}.
#' @param postcode_path Character. File path to postal areas (i.e. post codes) 
#'   shapefile, available \href{https://www.abs.gov.au/AUSSTATS/abs@@.nsf/DetailsPage/1270.0.55.003July 2011}{here}.
#' @param airport_beta Numeric. Parameter controlling the distribution of 
#'   tourists (international air passengers) around Australian international 
#'   airports. Distance to nearest airport is multiplied by this value and 
#'   exponentiated to give the relative density of tourists at a location. To 
#'   generate a distribution that ensures proportion \emph{p} of tourists 
#'   within distance \emph{d} of nearest airport, specify 
#'   \code{airport_beta=log(p)/d} (e.g. to have 50% of tourists within 200 km 
#'   of an airport, use \code{log(0.5)/200}). 
#' @param airport_tsi_beta Numeric. Interpretation is as for 
#'   \code{airport_beta}, but applies to air passengers arriving at Cairns 
#'   International Airport (CNS) from the Torres Strait Islands.
#' @return A \code{drake} plan containing targets that generate objects used
#'   across species.
#' @importFrom raster extent
#' @importFrom drake plan file_in file_out
#' @importFrom rlang !!
#' @importFrom tmap tmap_save tmap_arrange
#' @importFrom stats median
plan_globals <- function(clum_path, nvis_path, ndvi_path, fertiliser_data_path,
 nrm_path, containers_data_path, postcode_path,  airport_beta, 
 airport_tsi_beta) {
  
  # prepare extent and resolution
  # force national extent, 1km base res and 5km aggregated res for now
  extent <- c(-1888000, 2122000, -4847000, -1010000)
  output_resolution <- c(1000, 1000)
  output_resolution_agg <- c(5000, 5000)
  agg_factor <- output_resolution_agg[1]/output_resolution[1]
  
  major_aviation_terminals <-
    "risk_layers/pathway/raw_data/Major_Airports/MajorAviationTerminals.gdb"
  
  tourist_beds <-
    "risk_layers/pathway/raw_data/Tourist_Beds/TourBedsAlbers1km.shp"
  
  pop_density <- 
    "risk_layers/pathway/raw_data/Population/pop_density_1000m.tif"
  
  drake::drake_plan(
    # Download worldclim2
    download_worldclim = 
      download_worldclim2(
        outfile = drake::file_out('downloads/bioclim_10m.zip'),
        variable = 'bio',
        resolution = '10m'), # 10 arc-minutes
    
    # Extract zipfile
    extract_worldclim = 
      extract_worldclim2(
        path_2_zip = drake::file_in('downloads/bioclim_10m.zip'),
        outdir = drake::file_out("risk_layers/abiotic/bioclim_10m")),
    
    # Output res
    output_resolution = !!list(output_resolution),
    output_resolution_agg = !!list(output_resolution_agg),
    # ^ list avoids error: not that many frames on the stack
    
    # Output extent, passed as argument to this function
    output_extent = !!list(extent),
    # ^ list avoids error: not that many frames on the stack
    
    # Compress large categorical rasters into run-length encoded vectors for
    # rapid access and processing.
    compress_clum = 
      rle_compress(x=drake::file_in(!!clum_path), 
                   outfile=drake::file_out(
                     !!sprintf('risk_layers/biotic/processed/clum_rle_%s.rds', 
                               output_resolution[1]))),
    
    compress_nvis = 
      rle_compress(x=drake::file_in(!!nvis_path), 
                   outfile=drake::file_out(
                     !!sprintf('risk_layers/biotic/processed/nvis_rle_%s.rds',
                               output_resolution[1]))),
    
    
    # Create a land mask for Australia based on the CLUM raster, to be used when
    # distributing marine port weights across the continent.
    aus_mask_clum = 
      na_mask(infile=drake::file_in(!!clum_path),
              outfile=drake::file_out(!!sprintf(
                'risk_layers/auxiliary/aus_mask_clum_%s.tif', 
                output_resolution[1]
              )),
              res=output_resolution),
    
    # Create an empty Float32 raster that will be used as a template to 
    # initialise new raster datasets.
    make_template_float32 =
      initialise_raster(
        drake::file_in(!!clum_path),
        drake::file_out(!!sprintf(
          "risk_layers/auxiliary/clum_template_Float32_%s.tif", 
          output_resolution[1]
        )),
        res=output_resolution, datatype='Float32', 
        overwrite=TRUE),
    
    template_float32 = 
      drake::file_in(!!sprintf(
        "risk_layers/auxiliary/clum_template_Float32_%s.tif", 
        output_resolution[1]
      )),
    
    
    # Risk layers (non-species specific)
    tourist_rooms =
      rasterize_vector(
        vector_data = drake::file_in(!!tourist_beds),
        template_raster = template_float32,
        outfile = drake::file_out(!!sprintf(
          "risk_layers/pathway/processed/tourism_beds_%s.tif", 
          output_resolution[1]
        )),
        field = "TouRmEst",
        datatype = "Float32",
        overwrite = TRUE),
    
    airport_proximity =
      get_airport_dist(
        vector_data = drake::file_in(!!major_aviation_terminals),
        template_raster = drake::file_in(!!sprintf(
          'risk_layers/auxiliary/aus_mask_clum_%s.tif', 
          output_resolution[1]
        )),
        airport_codes = c("BNE","MEL","PER", "ADL","CNS","DRW", "SYD","CNB"),
        outfile = drake::file_out(!!sprintf(
          "risk_layers/pathway/processed/airport_proximity_%s.tif", 
          output_resolution[1]
        )),
        overwrite = TRUE),
    
    airport_distance_weight = 
      weight_airport_dist(
        airport_dist = drake::file_in(!!sprintf(
          "risk_layers/pathway/processed/airport_proximity_%s.tif", 
          output_resolution[1]
        )),
        beta = !!airport_beta,
        outfile = drake::file_out(!!sprintf(
          "risk_layers/pathway/processed/airport_dist_weight_%s.tif", 
          output_resolution[1]
        )),
        overwrite = TRUE),
    
    cairns_airport_proximity =
      get_airport_dist(
        vector_data = drake::file_in(!!major_aviation_terminals),
        template_raster = drake::file_in(!!sprintf(
          'risk_layers/auxiliary/aus_mask_clum_%s.tif', 
          output_resolution[1]
        )),
        outfile = drake::file_out(!!sprintf(
          "risk_layers/pathway/processed/cairns_airport_proximity_%s.tif", 
          output_resolution[1]
        )),
        airport_codes = "CNS",
        overwrite = TRUE),
    
    cairns_airport_distance_weight = 
      weight_airport_dist(
        airport_dist = drake::file_in(!!sprintf(
          "risk_layers/pathway/processed/cairns_airport_proximity_%s.tif", 
          output_resolution[1]
        )),
        beta = !!airport_tsi_beta,
        outfile = drake::file_out(!!sprintf(
          "risk_layers/pathway/processed/cairns_airport_dist_weight_%s.tif", 
          output_resolution[1]
        )),
        overwrite = TRUE),
    
    fertiliser_landuses =
      binarize_and_aggregate(
        rle = drake::file_in(!!sprintf(
          "risk_layers/biotic/processed/clum_rle_%s.rds", output_resolution[1]
        )),
        outfile = drake::file_out(!!sprintf(
          "outputs/not_pest_specific/fert_landuses_%s.tif", output_resolution[1]
        )), 
        categories = c(
          330, 331, 332, 333, 334, 335, 336, 337, 338, 
          340, 341, 342, 343, 344, 345, 346, 347, 348, 349, 
          350, 351, 352, 353, 
          420, 421, 423, 424, 
          430, 431, 432, 433, 434, 435, 436, 437, 438, 439, 
          440, 441, 442, 443, 444, 445, 446, 447, 448, 449, 
          450, 451, 452, 453, 454, 
          510, 511, 512, 513, 514),
        res = output_resolution,
        overwrite = TRUE),
    
    NRM_fertiliser =
      fertiliser_by_nrm(abs_data = drake::file_in(!!fertiliser_data_path), 
                        nrm_shapefile = drake::file_in(!!nrm_path)),
    
    fert_weight =
      fertiliser_weight(
        fert_nrm = NRM_fertiliser, 
        fert_landuses = drake::file_in(!!sprintf(
          "outputs/not_pest_specific/fert_landuses_%s.tif", output_resolution[1]
        )),
        outfile = drake::file_out(!!sprintf(
          "outputs/not_pest_specific/fert_weight_%s.tif", output_resolution[1]
        ))),
    
    container_weight = 
      container_weights(path = drake::file_in(!!containers_data_path),
                        sheet_nums = c(2:6),
                        range = "A7:M2217",
                        postcode_shp = drake::file_in(!!postcode_path),
                        na = c("", "-", "np")),
    
    NDVI_reproject =
      gdal_reproject(
        infile = drake::file_in(!!ndvi_path),
        outfile = drake::file_out(!!sprintf(
          "risk_layers/biotic/processed/NDVI_%s.tif", output_resolution[1]
        )),
        src_proj = "EPSG:4283", # GDA94
        tgt_proj = "EPSG:3577", # Australian Albers
        res = output_resolution,
        resampling_method = "bilinear",
        tgt_extent = output_extent[c(1, 3, 2, 4)],
        buffer = 5000),
    
    NDVI_normalised =
      min_max_normalize(
        rast = drake::file_in(!!sprintf(
          "risk_layers/biotic/processed/NDVI_%s.tif", output_resolution[1]
        )),
        outfile = drake::file_out(!!sprintf(
          "risk_layers/biotic/processed/NDVI_norm_%s.tif", output_resolution[1]
        ))),
    
    # Plots
    plot_tourist_rooms = 
      static_map(
        ras = drake::file_in(!!sprintf(
          "risk_layers/pathway/processed/tourism_beds_%s.tif", 
          output_resolution[1]
        )),
        xlim = c(112.76, 155),
        ylim = c(-44.03, -9.21),
        legend_title = "log10(Rooms)",
        set_value_range = c(0, Inf),
        scale_type = "log10",
        transparency = 1, 
        aggregate_raster = list(!!agg_factor, sum), 
        height = 6.5,
        outfile = drake::file_out(!!sprintf(
          "outputs/not_pest_specific/tourist_rooms_%s.pdf", 
          output_resolution_agg[1]
        ))),
    
    plot_pop_density = 
      static_map(
        ras = drake::file_in(!!pop_density), 
        xlim = c(112.76, 155),
        ylim = c(-44.03, -9.21),
        legend_title = "log10(Density)",
        set_value_range = c(0, Inf),
        scale_type = "log10",
        transparency = 1, 
        aggregate_raster = list(!!agg_factor, sum), 
        height = 7,
        outfile = drake::file_out(!!sprintf(
          "outputs/not_pest_specific/pop_density_%s.pdf", 
          output_resolution_agg[1]
        ))),
    
    plot_airport_distances = 
      tmap::tmap_save(
        tmap::tmap_arrange(
          static_map(
            ras = drake::file_in(!!sprintf(
              "risk_layers/pathway/processed/airport_proximity_%s.tif", 
              output_resolution[1]
            )),
            xlim = c(112.76, 155),
            ylim = c(-44.03, -9.21),
            height = 4,
            legend_title = "Distance (km)",
            transparency = 1, 
            aggregate_raster = list(5, max)
          ),
          static_map(
            ras = drake::file_in(!!sprintf(
              "risk_layers/pathway/processed/airport_dist_weight_%s.tif", 
              output_resolution[1]
            )),
            xlim = c(112.76, 155),
            ylim = c(-44.03, -9.21),
            height = 4,
            legend_title = "Weight score",
            transparency = 1, 
            aggregate_raster = list(!!agg_factor, max)
          ),
          nrow = 2), 
        filename = drake::file_out(!!sprintf(
          "outputs/not_pest_specific/airport_distance_%s.pdf", 
          output_resolution_agg[1]
        )),
        width = 6.5*300,
        height = 8*300),
    
    plot_cairns_airport_distances = 
      tmap::tmap_save(
        tmap::tmap_arrange(
          static_map(
            ras = drake::file_in(!!sprintf(
              "risk_layers/pathway/processed/cairns_airport_proximity_%s.tif", 
              output_resolution[1]
            )),
            xlim = c(138.9, 151.1),
            ylim = c(-20.17, -10.22),
            height = 4,
            legend_title = "Distance (km)",
            transparency = 1, 
            aggregate_raster = list(5, max)
          ),
          static_map(
            ras = drake::file_in(!!sprintf(
              "risk_layers/pathway/processed/cairns_airport_dist_weight_%s.tif", 
              output_resolution[1]
            )),
            xlim = c(138.9, 151.1),
            ylim = c(-20.17, -10.22),
            height = 4,
            legend_title = "Weight score",
            transparency = 1, 
            aggregate_raster = list(!!agg_factor, max)
          ),
          nrow = 2), 
        filename = drake::file_out(!!sprintf(
          "outputs/not_pest_specific/cairns_airport_distance_%s.pdf", 
          output_resolution_agg[1]
        )),
        width = 6.5*300,
        height = 8*300),
    
    plot_fert_weight = 
      static_map(
        ras = drake::file_in(!!sprintf(
          "outputs/not_pest_specific/fert_weight_%s.tif", output_resolution[1]
        )),
        xlim = c(112.76, 155),
        ylim = c(-44.03, -9.21),
        legend_title = 'log10(Prop. Tonnage)',
        set_value_range = c(0, Inf),
        transparency = 1,
        scale_type = 'log10',
        aggregate_raster = list(!!agg_factor, stats::median),
        height = 6.5,
        outfile = drake::file_out(!!sprintf(
          "outputs/not_pest_specific/fert_weight_%s.pdf", 
          output_resolution_agg[1]
        ))
      ),
    
    NDVI_plot =
      static_map(
        ras = drake::file_in(!!sprintf(
          "risk_layers/biotic/processed/NDVI_%s.tif", output_resolution[1]
        )),
        xlim = c(112.76, 155),
        ylim = c(-44.03, -9.21),
        legend_title = "NDVI",
        transparency = 1, 
        aggregate_raster = list(!!agg_factor, stats::median),
        height = 7,
        outfile = drake::file_out(!!sprintf(
          "outputs/not_pest_specific/NDVI_%s.pdf", output_resolution_agg[1]
        ))
      )
  )
}
