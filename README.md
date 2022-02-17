# An R package for creating Australian maps of establishment likelihood for terrestrial plant pests

`edmaps` is an *R* package that greatly simplifies the computational steps required for creating maps of establishment as a function of pathway arrival rates, climate suitability and host availability. 
It uses a framework developed in a recent CEBRA report [Camac et al. (2020) Developing pragmatic maps of establishment likelihood for plant pests](https://www.google.com/url?sa=t&rct=j&q=&esrc=s&source=web&cd=&cad=rja&uact=8&ved=2ahUKEwjgyqO2pIX2AhXUSWwGHYuzB-MQFnoECAkQAQ&url=https%3A%2F%2Fcebra.unimelb.edu.au%2F__data%2Fassets%2Fpdf_file%2F0012%2F3539397%2F170607_final_report.pdf&usg=AOvVaw1U7_yiV7SNTPK1wSWO86B0), which has since been expanded and updated in the latest CEBRA report [Camac et al. (2021) Using edmaps & Zonation to inform multi-pest early-detection surveillance designs](https://cebra.unimelb.edu.au/__data/assets/pdf_file/0009/3889773/20121001_final_report.pdf).


It works by using a user-defined *Microsoft Excel* spreadsheet (see below) to create a *make*-like workflow that automates the entire process from raw data processing through to the production of GeoTIFF rasters as well as presentation-quality static and interactive maps. 

Users greatly benefit from workflows using `edmaps` because they define the dependency structure of those objects, and ensure that after modifications (e.g., changes to files, R objects, function arguments), any affected dependants are updated. In order to create such workflows `edmaps` uses the [`drake`](https://docs.ropensci.org/drake/) package for *R*.

Specifically, `edmaps` automates the many steps that need to be taken in order to create pest-specific maps of establishment likelihoods. 
Broadly, these steps are grouped as follows:


1.  Download global climate data from [WorldClim](https://worldclim.org);
2.  Restructure large categorical raster datasets (landuse and NVIS) for efficient access;
3.  Prepare supporting datasets (e.g. a raster defining the Australian land mass);
4.  Create rasters describing the geographic distribution of arrivals for each pathway relevant to a pest;
5.  Create a raster defining the cell-level expected likelihood of arrivals across all relevant pathways;
6.  Create a raster defining biotic suitability for the pest (i.e. geographic distribution of host material);
7.  Create a raster defining abiotic suitability for a pest (if relevant). This may involve using a climate suitability layer provided by the user, or if not supplied, occurrence data will be sourced (from user-supplied files, from [GBIF](https://www.gbif.org), or both), automatically cleaned, and a range bagging model will be fit and projected across the landscape to estimate climatic suitability;
8.  Create a raster defining the establishment likelihood;
9.  Create a raster at coarser resolutions (for management or display purposes);
10. Create publication quality national and city-wide maps of pest establishment likelihoods and other intermediate maps (e.g. climate suitability, host distributions etc.);
11. Create national and sub national interactive html maps of estimated total pest arrivals, environmental suitability (abiotic x biotic) and establishment likelihoods; and
12. Create GeoTiffs of all output rasters.


## Preparing `edmaps`
`edmaps` estimated establishment likelihoods by harnessing data from a wide range of publicly available spatial datasets created by Australian governments and scientists. 
A collated repository of the required spatial layers needed for `edmaps` can be downloaded from [here](https://github.com/jscamac/edmaps_data_Australia).

Broadly this repository contains the following data types:

- **abiotic/**
  - occurrences/ (user-collated occurrence records and CABI data. Generally set up as a sub folder for each species. CABI distribution data is downloaded from CABI datasheets in csv format. For example see [here](https://www.cabi.org/isc/datasheet/27377).
- **auxiliary/** (a folder containing spatial layers for facilitating `edmaps` in identifying coastal zones and area of extent)
- **biotic/**
  - `citrus_native_hosts_example.tif` (An example native host layer created using `edmaps::rasterize_range()`)
  - raw_data/
    - ACLUM/ (Australian landuse raster);
    - NDVI/ (NDVI raster);
    - NVIS\_5.1/ (NVIS raster);
- **pathway/raw_data/**
  - Containers/ (contains POA shapefile \& `containers\_bypostcode.xls`)
  - Fertiliser/ (contains fertiliser use csv file and NRM shapefile)
  - Major_Airports/ (Australian airport locations)
  - Population/ (human population raster)
  - Ports/ (port locations/use csv file)
  - Tourist_Beds/ (tourist bed shapefile)
- **user_input/**
  - `parameters.xlsx` (An example file for specifying global and species parameters)
- `renv.lock` (A renv lock file that contains the R package dependencies required to run edmaps outside of docker)


To use `edmaps` first download this data repository. This can be done by either visiting the website, clicking `Code` and then `Download ZIP`. Or by using git to download the repository via the shell/terminal/Rstudio terminal:

```
git clone https://github.com/jscamac/edmaps_data_Australia.git
```

## Defining your workflow...
Once the data repository is downloaded you can access the `user_input/parameters.xlsx` workbook.
This excel workbook is the main user-defined file that defines the workflow that edmaps will implement.
The workbook contains two tabs one for specifying global parameters and another for specifying pest-specific parameters. 

### Global variables
The first tab (*Global variables*) defines the set of parameters that are relevant to all pests considered. 
These include various input data paths, GBIF account details, distance decay parameters and whether interactive html maps should be produced.

### Species-specific variables
The second tab (*Species-specific parameters*) contains pest-specific parameters, where each pest is a separate row in the spreadsheet. These parameters encompass:

- species name and whether it may form part of a threat group.
- which pathways are relevant to the pest;
- whether to incorporate abiotic suitability (may not be relevant for some pests);
- whether NDVI and/or NVIS should be incorporated when estimating biotic suitability;
- file paths to occurrence datasets and CABI distributional data or a user's own climate suitability layer;
- file paths to user pre-defined host distribution raster
- whether to exclude BIOCLIM variables from range bagging model (only relevant if climate suitability file not specified);
- 95% confidence bounds of annual leakage rates & pathway-level establishment probabilities
- Pathway specific distance penalties
- the scale at which outputs should be aggregated.

The excel workbook provides tooltips for providing additional information to allow the file to be correctly parametrised.

## Running `edmaps`

`edmaps` depends on multiple spatial libraries (e.g. gdal, proj) and *R* packages to be installed. 
While these can be manually installed, we advise that this is done as a last resort. 
This is because each computer is different, likely using different versions of spatial libraries or R packages. Recently there has been considerable changes to many spatial R packages and libraries which have prevented backwards compatibility. As such it is extremely difficult ensure `edmaps` will run in all settings.

For this reason, and to ensure `edmaps` functionality into perpetuity, we have designed `edmaps` to be implemented via a [*docker*](https://www.docker.com/) container. 
*Docker* is the world’s leading software container platform that is used to create lightweight, self-contained virtual Linux systems that contain all relevant open source software required to run developed software. 
Unlike other virtual machines, _Docker_ does not bundle a full operating system. 
Rather it only installs libraries and settings required to make the developed software work. 
This means that *Docker* can be used to eliminate the “it works on my machine” problems when running software. 
The other major advantage is that it removes the need for users to install software dependencies, as the hard work is already done.

In the following sections we outline three methods for running `edmaps`, given that `parameters.xlsx` has been filled out. 
While we recommend the *Docker* approach, we acknowledge that not all users will have the appropriate permissions to do so. 
As such we also outline one additional approach for implementation using the *R* package `renv`. This approach will ensure the correct versions of *R* pacakge dependencies are installed, however, it cannot ensure the correct version of system requirements (e.g. *R*, *Java*, *pandoc*, *proj* and *gdal* libraries) are installed.


## Using `edmaps` with Docker (recommended)
We have created a *Docker* image that contains all the system libraries, software (e.g. *R*, *Java*, *pandoc*) and *R* packages 
required to install and run workflows produced by `edmaps`.

To use the image, first install [Docker](https://docs.docker.com/get-started/) onto your machine. 
When installing *Docker* on *Windows*, you will be prompted to select whether to use Linux or *Windows* containers. 
Leave this at it's default (i.e. to use Linux containers). Once installed, we recommend users set *Docker* settings 
such that containers have access to at least 16 GB of RAM, and a specified number of CPUs (We recommend that users allow access up to all but one core for processing workflows derived from `edmaps`, especially if being used for many pests).

Next we can download an `edmaps` virtual machine by entering the following into the command line using Command Prompt or the terminal (requires internet connection):

```
docker pull jscamac/edmaps
```

Once the *Docker* image has been successfully downloaded, use the Command Prompt or terminal to navigate to the local copy of the data directory outlined above. 
Next run the system-specific command line:

**macOS & Linux**

```
docker run -d -v $(pwd):/home/rstudio/ -p 127.0.0.1:8787:8787 \
-e DISABLE_AUTH=true jscamac/edmaps
```

**NOTE:** Windows users may need to replace $(pwd) with the path to the downloaded repository or possibly %cd%.

The above command line will launch a virtual machine containing a local RStudio server that has access to your data directory. You can open this Rstudio session by opening your web browser and navigating to the following address: `localhost:8787/`

Once you can see the Rstudio server, and you've specified your parameters in the `user_input/parameters.xlsx` workbook, you can now build your R workflow copying the following code into the *R* console:

```
## Prevent rJava issue with OpenStreetMaps
Sys.setenv(NOAWT=1)

## Load edmaps
library(edmaps)

# Set drake options.
# - Run make interactively
options(drake_make_menu = FALSE)

# - Set up parallel processing
future::plan(future.callr::callr)

# Build edmaps plan based on input workbook
edmaps_plan <- edmaps::excel_to_plan(file = 'user_input/parameters.xlsx')
```

Next we can run the workflow by doing the following:

```
drake::make(edmaps_plan, retries=3)
```

This will then process the raw spatial files, estimate establishment likelihoods and produce static and interactive maps as well as GIS-compatible rasters (GeoTIFF) can be found within the `outputs/` directory. Depending on both how much computing resources you have given Docker access to, as well as the number of species you are estimating establishment likelihoods, this may take anywhere from 30 minutes to a couple of hours to run.

If you would like to generate a log of the entire workflow used by edmaps based on your parameter inputs, you can obtain that by running:

```
drake::plan_to_code(edmaps_plan, 'plan.log')
```


## Running `edmaps` on local computer with `renv`


If insufficient privileges exist to use _Docker_, an alternative is to use a [`renv`](https://rstudio.github.io/renv/articles/renv.html) (**r**eproducible **env**ironments) workflow.
The `renv` *R* package reproduces a pre-defined package environment, ensuring that specified package versions are used. To do this it creates a private, isolated package library for an *R* project, 
and obtains defined versions of packages from defined repositories. 
Versions and repositories are specified in a "lock file" (`renv.lock`), and the lock file describing the versions
used for the case studies has been provided with this report. Since the `renv` package library is project-specific, 
it will not overwrite or interfere with package versions used in other projects.

The most important difference between the *Docker* approach described above, and *renv*, is that the latter manages *R* packages only; required system libraries and tools must be installed manually.

Assuming software dependencies (e.g. *Java*, *R*) are available, recreating the package environment with `renv` is straightforward.

First, navigate to the data directory (see above) using R/Rstudio. Then in the console run:

```
# Install renv if necessary
if(!requireNamespace('renv', quietly=TRUE) || packageVersion('renv') != '0.15.2') {
  if(!dir.exists('lib')) dir.create('lib')
  install.packages(
    'renv', repos = 'https://cran.microsoft.com/snapshot/2022-02-10', lib='lib')
  library(renv, lib.loc='lib')
}

# Set up package environment
renv::consent(provided=TRUE)
renv::init(bare=TRUE, settings=list(use.cache=FALSE))
renv::restore(clean=TRUE, prompt=FALSE)
```

This will create the local R package environment within the data repository. Once this has installed all R dependencies, you should be able to run build the workflow and run `edmaps` in your own R session using the following the same code as above.


## Troubleshooting?

If you are having issues implementing `edmaps` feel free to create an issue here outlining your problem. I'll endeavour to resolve it ASAP.
