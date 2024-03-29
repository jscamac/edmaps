% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/species_plan.R
\name{species_plan}
\alias{species_plan}
\title{Generate a drake plan for a species}
\usage{
species_plan(
  species,
  clum_classes,
  nvis_classes,
  host_path,
  pathways,
  include_abiotic_weight = TRUE,
  climate_suitability_path,
  exclude_bioclim_vars = NULL,
  include_ndvi = TRUE,
  aggregated_res = c(5000, 5000),
  make_interactive_maps = TRUE,
  processed_data_path,
  clum_path,
  nvis_path,
  ndvi_path,
  pop_density_path,
  airports_path,
  tourist_beds_path,
  airport_beta = log(0.5)/200,
  airport_tsi_beta = log(0.5)/10,
  port_data_path,
  port_weight_beta,
  fertiliser_data_path,
  nrm_path,
  containers_data_path,
  postcode_path,
  occurrence_path,
  infected_countries,
  cabi_path,
  use_gbif = FALSE,
  gbif_species,
  gbif_min_year = 1970,
  gbif_max_uncertainty = 20000,
  gbif_username,
  gbif_password,
  basemap_mode = c("osm", "boundaries"),
  minimum_probability_for_maps = 1e-05,
  manual_check_flagged_records = FALSE,
  wind_effect_width,
  leakage_tourists,
  establishment_tourists,
  leakage_returning,
  establishment_returning,
  leakage_torres,
  establishment_torres,
  leakage_mail,
  establishment_mail,
  leakage_vessels,
  establishment_vessels,
  leakage_fertiliser,
  establishment_fertiliser,
  leakage_machinery,
  establishment_machinery,
  leakage_containers,
  establishment_containers,
  leakage_nurserystock,
  establishment_nurserystock,
  leakage_food,
  establishment_food,
  leakage_goods,
  establishment_goods,
  leakage_northwind,
  establishment_northwind,
  leakage_pacificwind,
  establishment_pacificwind,
  leakage_nzwind,
  establishment_nzwind,
  overwrite = TRUE
)
}
\arguments{
\item{species}{The name of the species. This will be used for naming output
files and folders.}

\item{clum_classes}{An integer vector indicating which ACLUM classes are
considered host plants for \code{species}. Either \code{clum_classes} or
\code{nvis_classes} (or both) must be provided. If \code{user_host_path} is
also provided, the union of the two datasets will be used to define host
distribution.}

\item{nvis_classes}{An integer vector indicating which NVIS classes are
considered host plants for \code{species}. Either \code{clum_classes} or
\code{nvis_classes} (or both) must be provided.}

\item{host_path}{Character. Optional file path to a raster dataset that
describes the distribution of host material for the species. The file must
be readable by GDAL. All cell values other than zero or NA will be
considered to be host material. Dataset will be projected and resampled as
necessary to match the spatial extent and resolution of analysis. If
\code{clum_classes} is also provided, the union of the two datasets will be
used to define host distribution. A valid coordinate reference system must
be associated with the spatial dataset.}

\item{pathways}{A character vector of invasion pathways that should be
included. Can be one or more of: \code{'containers'}, \code{'fertiliser'},
\code{'food'}, \code{'goods'}, \code{'machinery'}, \code{'mail'},
\code{'nurserystock'}, \code{'residents'}, \code{'torres'},
\code{'tourists'}, \code{'vessels'}, \code{'northwind'},
\code{'pacificwind'}, \code{'nzwind'}.}

\item{include_abiotic_weight}{Logical. Should suitability be dependent on
climate? Considered TRUE if \code{climate_suitability_path} is provided,
or if \code{use_gbif} is TRUE.}

\item{climate_suitability_path}{Optional file path to a raster describing
climatic suitability across the landscape. If provided, the raster must be
have the Australian Albers coordinate system (EPSG:3577), spatial
resolution of 1000 m, and must have xmin = -1888000, xmax = 2122000,
ymin =-4847000, ymax = -1010000. If not provided and
\code{include_abiotic_weight} is TRUE, a range bag model will be fit to
estimate climatic suitability.}

\item{exclude_bioclim_vars}{Character vector of bioclim variables that
should not be used when fitting a range bag model (see
\code{\link[=range_bag]{range_bag()}}) of climatic suitability. Variables should be
specified as, e.g., \code{c("bio01", "bio12")}. Ignored if
\code{climate_suitability_path} is provided.}

\item{include_ndvi}{Logical. Should biotic suitability be dependent on NDVI?}

\item{aggregated_res}{A numeric vector of 2 elements, indicating the desired
resolution of aggregated establishment likelihood rasters, in metres.}

\item{make_interactive_maps}{Logical. Should interactive html maps be
generated?}

\item{processed_data_path}{Path to a directory that will be used to store
processed datasets. This will be created, recursively, if it does not
exist.}

\item{clum_path}{Path to the ACLUM raster.}

\item{nvis_path}{Path to the NVIS raster.}

\item{ndvi_path}{Path to the NDVI raster.}

\item{pop_density_path}{Path to the population density raster.}

\item{airports_path}{Path to the major airports vector dataset.}

\item{tourist_beds_path}{Path to the tourist beds vector dataset.}

\item{airport_beta}{Numeric. Parameter controlling the distribution of
international tourists passengers around international airport. Default is
\code{log(0.5)/200} (i.e. 50\% of passengers within 200km of airport).}

\item{airport_tsi_beta}{Numeric. Parameter controlling the distribution of
Torres Strait passengers around Cairns airport. Default is
\code{log(0.5)/10} (i.e., 50\% of passengers within 10km of Cairns
airport).}

\item{port_data_path}{File path to the marine ports .csv file.}

\item{port_weight_beta}{Numeric. Defines the decay rate of an exponential
model. In the context of pests entering via the vessel pathway, this
reflects the decrease in the relative likelihood of pest arrival at
locations distant from marine ports. For example,
\code{prob_weight_beta=log(0.5)/10} would lead to distance-decay that
leads to 50\% (i.e. \code{0.5}) of establishment likelihood (prior to
considering other relevant pathways) within a distance of \code{10} map
units (i.e., 10 kilometres when \code{res} is 1000).}

\item{fertiliser_data_path}{File path to a csv file containing information
about fertiliser usage by NRM.}

\item{nrm_path}{File path to a polygon shapefile of NRMs (natural resource
management areas).}

\item{containers_data_path}{File path to the dataset giving the distribution
of containers by postcode.}

\item{postcode_path}{File path to postal areas shapefile.}

\item{occurrence_path}{Path to a .csv file containing occurrence data. Must
include columns \code{Longitude} and \code{Latitude}. Coordinates are
expected to be in decimal degrees (WGS84).}

\item{infected_countries}{A character vector of countries within which the
\code{species} occurs. Ignored if \code{climate_suitability_path} is
provided.Only one of \code{infected_countries} or \code{cabi_path} should
be provided.}

\item{cabi_path}{Path to a .csv file downloaded from CABI, indicating the
countries within which the \code{species} occurs. Download links to these
files can be found at the bottom of CABI species datasheet webpages, e.g.
https://www.cabi.org/isc/datasheet/17685. Ignored if
\code{climate_suitability_path} is provided. Only one of
\code{infected_countries} or \code{cabi_path} should be provided.}

\item{use_gbif}{Logical. Should species occurrence records be sourced from
GBIF? Ignored if \code{climate_suitability_path} is provided.}

\item{gbif_species}{Character vector. Taxon names to use when querying GBIF.
Ignored if \code{climate_suitability_path} is provided.}

\item{gbif_min_year}{Integer. The minimum year (\code{yyyy}) to be included
when downloading GBIF data. Ignored if \code{climate_suitability_path} is
provided.}

\item{gbif_max_uncertainty}{Numeric. The maximum permissable coordinate
uncertainty for GBIF records. Ignored if \code{climate_suitability_path} is
provided.}

\item{gbif_username}{GBIF username to use for querying GBIF's occurrence
download API endpoint. If missing, the less efficient "search" endpoint
is used. Register at http://gbif.org.}

\item{gbif_password}{GBIF password to use for querying GBIF's occurrence
download API endpoint. If missing, the less efficient "search" endpoint
is used. Register at http://gbif.org.}

\item{basemap_mode}{Type of basemap for static maps. Either \code{'osm'}
(default), or \code{'boundaries'} (polygons delineating borders of
states/territories).}

\item{minimum_probability_for_maps}{Numeric. A value between 0 and 1,
defining the minimum establishment probability to be displayed on plotted
maps. Values below this threshold will be excluded. Default is \code{1E-5}.}

\item{manual_check_flagged_records}{Logical. Should an interactive map be
used for manually checking flagged occurrence records? If \code{TRUE}, the
user will have the opportunity to select dubious points (i.e. occurrences
in countries for which CABI has no record of the species' establishment),
to be retained. If \code{FALSE} (the default), all such dubious points will
be excluded. Ignored if \code{climate_suitability_path} is provided. Note
that manual checking is not possible when using \code{\link[=excel_to_plan]{excel_to_plan()}} since the
required interactivity will interrupt plan processing.}

\item{wind_effect_width}{Numeric. For wind pathways, the distance (in metres)
inland from the coastline, over which wind-based arrival applies. E.g. if
pests are expected to be carried by wind up to 50 km inland from the coast,
use \code{50000}. Ignored if \code{pathways} does not include one or more
of \code{northwind}, \code{pacificwind}, or \code{nzwind}.}

\item{leakage_tourists, leakage_returning, leakage_torres, leakage_mail, leakage_vessels, leakage_fertiliser, leakage_machinery, leakage_containers, leakage_nurserystock, leakage_food, leakage_goods, leakage_northwind, leakage_pacificwind, leakage_nzwind}{Numeric vector with length = 2, giving the range (bounds of 95\% CI) of the
number of leakage events per year for the pathway.}

\item{establishment_tourists, establishment_returning, establishment_torres, establishment_mail, establishment_vessels, establishment_fertiliser, establishment_machinery, establishment_containers, establishment_nurserystock, establishment_food, establishment_goods, establishment_northwind, establishment_pacificwind, establishment_nzwind}{Numeric vector with length = 2, giving the bounds of the 95\% CI of the rate
of survival & establishment to end of pathway, for leakage events on the
pathway.}

\item{overwrite}{Logical. Should the code executed by the resulting plan be
allowed to overwrite existing raster files? Default is \code{TRUE}.}
}
\description{
Generate a drake plan that facilitates reproducible generation of species
outputs.
}
\details{
To simplify reproducibility, \code{edmaps} provides an
\emph{Excel} interface for specifying species parameters relevant to
estimating establishment likelihood. An example spreadsheet is
bundled with the package, available at the path given by
\code{system.file('extdata/parameters.xlsx', package='edmaps')}. The
spreadsheet has two sheets, the first specifying "global" parameters
that will apply to all species (e.g. file paths to rasters that will),
be used regardless of species identity and the second specifying
parameters that can vary by species. In the second sheet, each row
corresponds to a separate species. Tooltips and data validation
guide the user with respect to expected/allowable data.
}
\seealso{
\code{\link[=excel_to_plan]{excel_to_plan()}}
}
