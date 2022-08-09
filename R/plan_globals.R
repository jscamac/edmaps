#' Create a drake plan describing targets relating to global parameters
#'
#' Create a drake plan that describes targets relating to global parameters
#' to be used by individual species plans.
#' @param clum_path Character. File path to [Catchment Scale Land Use of Australia (ACLUM)](http://www.agriculture.gov.au/abares/aclump/Pages/land-use/Catchment-scale-land-use-of-Australia-2018.aspx) raster.
#' @param nvis_path Character. File path to the [National Vegetation Information System (NVIS)](https://www.environment.gov.au/land/native-vegetation/national-vegetation-information-system/data-products) raster dataset.
#' @param ndvi_path Character. File path to the [Normalised Difference Vegetation Index (NDVI)](http://www.bom.gov.au/jsp/awap/ndvi/index.jsp?colour=colour&time=history/nat/2018110120190430&step=7&map=ndviave&period=6month&area=nat) raster dataset.
#' @param fertiliser_data_path Character. File path to a csv containing data
#'   describing fertiliser use.
#' @param nrm_path Character. File path to shapefile of Natural Resource
#'   Management (NRM) Regions.
#' @param containers_data_path Character. File path to xlsx file containing
#'   data about shipping container movements.
#' @param postcode_path Character. File path to postal areas (i.e. post codes)
#'   shapefile.
#' @param pop_density_path Character. File path to population density raster.
#' @param tourist_beds_path Character. File path to tourist beds shapefile.
#' @param airports_path Character. File path to major aviation terminals
#'   shapefile.
#' @param airport_beta Numeric. Parameter controlling the distribution of
#'   tourists (international air passengers) around Australian international
#'   airports. Distance to nearest airport is multiplied by this value and
#'   exponentiated to give the relative density of tourists at a location. To
#'   generate a distribution that ensures proportion _p_ of tourists
#'   within distance _d_ of nearest airport, specify
#'   `airport_beta=log(p)/d` (e.g. to have 50% of tourists within 200 km
#'   of an airport, use `log(0.5)/200`).
#' @param airport_tsi_beta Numeric. Interpretation is as for
#'   `airport_beta`, but applies to air passengers arriving at Cairns
#'   International Airport (CNS) from the Torres Strait Islands.
#' @param basemap_mode Type of basemap for static maps. Either `'osm'`
#'   (default), or `'boundaries'` (polygons delineating borders of
#'   states/territories).
#' @param processed_data_path Character. File path to directory for storing
#'   processed and intermediate datasets.
#' @details Datasets used by this function are available in the [_edmaps data
#'   Australia_ repository](https://github.com/jscamac/edmaps_data_Australia).
#' @return A `drake` plan containing targets that generate objects used
#'   across species.
#' @importFrom drake drake_plan file_in file_out
#' @importFrom rlang !!
#' @importFrom tmap tmap_save tmap_arrange
#' @importFrom stats median
plan_globals <- function(clum_path, nvis_path, ndvi_path, fertiliser_data_path,
                         nrm_path, containers_data_path, postcode_path,
                         pop_density_path, tourist_beds_path, airports_path, airport_beta,
                         airport_tsi_beta, basemap_mode=c('osm', 'boundaries'),
                         processed_data_path) {

  if(!dir.exists(processed_data_path)) {
    dir.create(processed_data_path, recursive=TRUE)
  }

  # prepare extent and resolution
  # force national extent, 1km base res and 5km aggregated res for now
  extent <- c(-1888000, 2122000, -4847000, -1010000)
  output_resolution <- c(1000, 1000)
  output_resolution_agg <- c(5000, 5000)
  agg_factor <- output_resolution_agg[1]/output_resolution[1]

  drake::drake_plan(
    # Download worldclim2
    download_worldclim =
      download_worldclim2(
        outfile = drake::file_out(!!sprintf('%s/bioclim_10m.zip', processed_data_path)),
        variable = 'bio',
        resolution = '10m'), # 10 arc-minutes

    # Extract zipfile
    extract_worldclim =
      extract_worldclim2(
        path_2_zip = drake::file_in(!!sprintf('%s/bioclim_10m.zip', processed_data_path)),
        outdir = drake::file_out(!!sprintf("%s/bioclim_10m", processed_data_path))),

    # Output res
    output_resolution = !!output_resolution,
    output_resolution_agg = !!output_resolution_agg,

    # Output extent, passed as argument to this function
    output_extent = !!extent,

    # Compress large categorical rasters into run-length encoded vectors for
    # rapid access and processing.
    compress_clum =
      rle_compress(x=drake::file_in(!!clum_path),
                   outfile=drake::file_out(
                     !!sprintf('%s/clum_rle_%s.rds', processed_data_path,
                               output_resolution[1]))),

    compress_nvis =
      rle_compress(x=drake::file_in(!!nvis_path),
                   outfile=drake::file_out(
                     !!sprintf('%s/nvis_rle_%s.rds', processed_data_path,
                               output_resolution[1]))),


    # Create a land mask for Australia based on the CLUM raster, to be used when
    # distributing marine port weights across the continent.
    aus_mask_clum =
      na_mask(infile=drake::file_in(!!clum_path),
              outfile=drake::file_out(!!sprintf(
                '%s/aus_mask_clum_%s.tif', processed_data_path,
                output_resolution[1]
              )),
              res=output_resolution),

    # Create an empty Float32 raster that will be used as a template to
    # initialise new raster datasets.
    make_template_float32 =
      initialise_raster(
        drake::file_in(!!clum_path),
        drake::file_out(!!sprintf(
          "%s/clum_template_Float32_%s.tif", processed_data_path,
          output_resolution[1]
        )),
        res=output_resolution, datatype='Float32',
        overwrite=TRUE),

    template_float32 =
      drake::file_in(!!sprintf(
        "%s/clum_template_Float32_%s.tif", processed_data_path,
        output_resolution[1]
      )),


    # Risk layers (non-species specific)
    tourist_rooms =
      rasterize_vector(
        vector_data = drake::file_in(!!tourist_beds_path),
        template_raster = template_float32,
        outfile = drake::file_out(!!sprintf(
          "%s/tourism_beds_%s.tif", processed_data_path,
          output_resolution[1]
        )),
        field = "TouRmEst",
        datatype = "Float32",
        overwrite = TRUE),

    airport_proximity =
      get_airport_dist(
        vector_data = drake::file_in(!!airports_path),
        template_raster = drake::file_in(!!sprintf(
          '%s/aus_mask_clum_%s.tif', processed_data_path, output_resolution[1]
        )),
        airport_codes = c("BNE","MEL","PER", "ADL","CNS","DRW", "SYD","CNB"),
        outfile = drake::file_out(!!sprintf(
          "%s/airport_proximity_%s.tif", processed_data_path,
          output_resolution[1]
        )),
        overwrite = TRUE),

    airport_distance_weight =
      weight_airport_dist(
        airport_dist = drake::file_in(!!sprintf(
          "%s/airport_proximity_%s.tif", processed_data_path,
          output_resolution[1]
        )),
        beta = !!airport_beta,
        outfile = drake::file_out(!!sprintf(
          "%s/airport_dist_weight_%s.tif", processed_data_path,
          output_resolution[1]
        )),
        overwrite = TRUE),

    cairns_airport_proximity =
      get_airport_dist(
        vector_data = drake::file_in(!!airports_path),
        template_raster = drake::file_in(!!sprintf(
          '%s/aus_mask_clum_%s.tif', processed_data_path, output_resolution[1]
        )),
        outfile = drake::file_out(!!sprintf(
          "%s/cairns_airport_proximity_%s.tif", processed_data_path,
          output_resolution[1]
        )),
        airport_codes = "CNS",
        overwrite = TRUE),

    cairns_airport_distance_weight =
      weight_airport_dist(
        airport_dist = drake::file_in(!!sprintf(
          "%s/cairns_airport_proximity_%s.tif", processed_data_path,
          output_resolution[1]
        )),
        beta = !!airport_tsi_beta,
        outfile = drake::file_out(!!sprintf(
          "%s/cairns_airport_dist_weight_%s.tif", processed_data_path,
          output_resolution[1]
        )),
        overwrite = TRUE),

    fertiliser_landuses =
      binarize_and_aggregate(
        rle = drake::file_in(!!sprintf(
          "%s/clum_rle_%s.rds", processed_data_path, output_resolution[1]
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
          "%s/NDVI_%s.tif", processed_data_path, output_resolution[1]
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
          "%s/NDVI_%s.tif", processed_data_path, output_resolution[1]
        )),
        outfile = drake::file_out(!!sprintf(
          "%s/NDVI_norm_%s.tif", processed_data_path, output_resolution[1]
        ))),

    # Plots
    plot_tourist_rooms =
      static_map(
        ras = drake::file_in(!!sprintf(
          "%s/tourism_beds_%s.tif", processed_data_path, output_resolution[1]
        )),
        xlim = c(112.76, 155),
        ylim = c(-44.03, -9.21),
        basemap_mode = !!basemap_mode,
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
        ras = drake::file_in(!!pop_density_path),
        xlim = c(112.76, 155),
        ylim = c(-44.03, -9.21),
        basemap_mode = !!basemap_mode,
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
              "%s/airport_proximity_%s.tif", processed_data_path,
              output_resolution[1]
            )),
            xlim = c(112.76, 155),
            ylim = c(-44.03, -9.21),
            basemap_mode = !!basemap_mode,
            height = 4,
            legend_title = "Distance (km)",
            transparency = 1,
            aggregate_raster = list(5, max)
          ),
          static_map(
            ras = drake::file_in(!!sprintf(
              "%s/airport_dist_weight_%s.tif", processed_data_path,
              output_resolution[1]
            )),
            xlim = c(112.76, 155),
            ylim = c(-44.03, -9.21),
            basemap_mode = !!basemap_mode,
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
              "%s/cairns_airport_proximity_%s.tif", processed_data_path,
              output_resolution[1]
            )),
            xlim = c(138.9, 151.1),
            ylim = c(-20.17, -10.22),
            basemap_mode = !!basemap_mode,
            height = 4,
            legend_title = "Distance (km)",
            transparency = 1,
            aggregate_raster = list(5, max)
          ),
          static_map(
            ras = drake::file_in(!!sprintf(
              "%s/cairns_airport_dist_weight_%s.tif", processed_data_path,
              output_resolution[1]
            )),
            xlim = c(138.9, 151.1),
            ylim = c(-20.17, -10.22),
            basemap_mode = !!basemap_mode,
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
        basemap_mode = !!basemap_mode,
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
          "%s/NDVI_%s.tif", processed_data_path, output_resolution[1]
        )),
        xlim = c(112.76, 155),
        ylim = c(-44.03, -9.21),
        basemap_mode = !!basemap_mode,
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
