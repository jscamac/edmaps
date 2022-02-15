# An R package for creating Australian maps of establishment likelihood for terrestrial plant pests

`edmaps` is an *R* package that greatly simplifies the computational steps required for creating maps of establishment as a function of pathway arrival rates, climate suitability and host availablity. 
It uses a framework proposed in a recent CEBRA report by Camac et al. (2019).
It works by using a user-defined *Microsoft Excel* spreadsheet (see below) to create a *make*-like workflow that automates the entire process from raw data processing through to the production of GeoTIFF rasters as well as presentation-quality static and interactive maps. 
Users greatly benefit from workflows using `edmaps` because they define the dependency structure of those objects, and ensure that after modifications (e.g., changes to files, R objects, function arguments), any affected dependants are updated. In order to create such workflows `edmaps` uses the [`drake`](https://docs.ropensci.org/drake/) package for *R*.

Specifically, `edmaps` automates the many steps that need to be taken in order to create pest-specific maps of establishment likelihoods. 
Broadly, these steps are grouped as follows:


1.  Download global climate data from [WorldClim](https://worldclim.org);
2.  Restructure large categorical raster datasets (landuse and NVIS) for efficient access;
3.  Prepare supporting datasets (e.g. a raster defining the Australian land mass);
4.  Create rasters describing the geographic distribution of arrivals for each pathway relevant to a pest;
5.  Create a raster defining the expected number of arrivals across all relevant pathways;
6.  Create a raster defining biotic suitability for the pest (i.e. geographic distribution of host material);
7.  Create a raster defining abiotic suitability for a pest (if relevant). This may involve using a climate suitability layer provided by the user, or if not supplied, occurrence data will be sourced (from user-supplied files, from [GBIF](https://www.gbif.org), or both), automatically cleaned, and a range bagging model will be fit and projected across the landscape to estimate climatic suitability;
8.  Create a raster defining the establishment likelihood;
9.  Create a raster at coarser resolutions (for management or display purposes);
10. Create publication quality national and city-wide maps of pest establishment likelihoods and other intermediate maps (e.g. climate suitability, host distributions etc.);
11. Create national interactive html maps of estimated total pest arrivals, environmental suitability (abiotic x biotic) and establishment likelihoods; and
12. Create GeoTiffs of all output rasters.


## System requirements
- *R* version 4.0.0 or newer
- 16 GB available RAM
- 10 GB available hard disk space
- Internet connection
- *Java* (JDK)
- [pandoc](https://pandoc.org/installing.html) (allows standalone html maps; if missing, a directory of accessory files will be created alongside the html file.)


### Operating system specific requirements


**Windows**


- [Rtools](https://cran.r-project.org/bin/windows/Rtools/) may be required to build packages from source.


**macOS**

- [Command Line Tools](http://osxdaily.com/2014/02/12/install-command-line-tools-mac-os-x/) to build packages from source when required.
- *macOS* is prone to errors when attempting to link _R_ and _Java_. These errors can often be resolved by running `sudo R CMD javareconf` in a terminal (requires administrator privileges).


**Linux**

On Linux, system packages must be installed from source, meaning that additional software and system libraries are required. The list below gives _Ubuntu_ package names. If using a different distribution, you will need to identify the corresponding package names (e.g.\ for some distributions, `dev` should be replaced with `devel`).

- libxml2-dev
- libudunits2-dev
- libgeos-dev
- libgdal-dev
- default-jdk
- liblzma-dev
- libbz2-dev
- libicu-dev
- libpcre3-dev
- libz-dev
- libcairo2-dev
- gdal-bin


### Required *R* packages

The `edmaps` package depends upon a set of other *R* packages in order to calculate likelihoods of establishment. 
Package functionality often changes as packages evolve, so specific versions of these dependencies (i.e.\ current versions as at 30 September, 2019) were used in the development of `edmaps` and its application to the case study species explored in this report. 
Binaries or source tarballs of these package versions are archived by the [MRAN (Microsoft R Application Network) Time Machine](https://mran.microsoft.com/timemachine), and are available at [http://cran.microsoft.com/snapshot/2019-09-30/](http://cran.microsoft.com/snapshot/2019-09-30/). To ensure expected behaviour and accurate reproduction of outputs, these package versions are recommended. Similarly, recursive dependencies (i.e.,\ dependencies of the direct dependencies, and so on) should also be installed from the above snapshot. Installation of the appropriate versions of dependencies can be automated, as described \hyperref[recreatingenv]{below}.

- CoordinateCleaner 2.0-15
- countrycode 1.2.0
- dplyr 1.0.0
- drake 7.12.4
- fasterize 1.0.2
- furrr 0.1.0
- future.callr 0.5.0
- gdalUtilities 1.1.0
- geometry 0.4.5
- ggplot2 3.3.2
- glue 1.4.1
- htmlwidgets 1.5.1
- knitr 1.29
- leafem 0.1.1
- leaflet 2.0.3
- leaflet.opacity 0.1.0
- lubridate 1.7.9
- magrittr 1.5
- mapedit 0.6.0
- OpenStreetMap 0.3.4
- purrr 0.3.4
- raster 3.3-7
- rasterVis 0.48
- readr 1.3.1
- readxl 1.3.1
- rgbif 3.1.0
- rlang 0.4.7
- rmarkdown 2.3
- rnaturalearth 0.1.0
- rnaturalearthdata 0.1.0
- rnaturalearthhires 0.2.0
- sf 0.9-4
- sp 1.4-2
- stars 0.4-3
- styler 1.3.2
- testthat 2.3.2
- tidyr 1.1.0
- tmap 3.0
- tmaptools 3.0
- viridis 0.5.1


### Data directory  (Not public)

`edmaps` requires a data directory (approximately 0.5 GB in size, not publicly available) to be present on your computer. 
This data directory contains all the raw spatial layers and data needed to create maps of establishment likelihood for Australia. The general structure and content of this directory are as follows:

- **risk_layers**
  - abiotic/occurrences/ (user-collated occurrence records and CABI data. Generally set up as a sub folder for each species. CABI distribution data is downloaded from CABI datasheets in csv format. For example see [here](https://www.cabi.org/isc/datasheet/27377).
  - biotic/raw_data/
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
- **user_input/**`parameters.xlsx` (file for specifying global and species parameters)
- `edmaps_1.4.0.tar.gz` (tarball for installing `edmaps`)
- `make.R` (Script for running `edmaps` without `renv`)
- `make_renv.R` (Script for running `edmaps` with `renv`)
- `renv.lock` (`renv` lock file containing package dependency details)

## User interface
In order to use this package. One must have access to the data directory described below. Currently these data are not all publicly available and must be requested from the Australian federal department of agriculture.

`edmaps` works by using a *Microsoft Excel* workbook to decide which steps are required and how they should be undertaken. 
This workbook can be found within the data subdirectory, `user_input/parameters.xlsx` (see above). 
The workbook contains two tabs one for specifying global parameters and another for specifying pest-specific parameters. 

### Global variables
The first tab (*Global variables*) defines the set of parameters that are relevant to all pests considered. 
These include various input data paths, the number of carriers entering Australia per annum for each pathway, air passenger distance decay penalties, and whether interactive html maps should be produced.

### Species-specific variables
The second tab (*Species-specific parameters*) contains pest-specific parameters, where each pest is a separate row in the spreadsheet. These parameters encompass:

- which pathways are relevant to the pest;
- whether to incorporate abiotic suitability (may not be relevant for some pests);
- whether NDVI and/or NVIS should be incorporated when estimating biotic suitability;
- file paths to occurrence datasets and CABI distributional data or a user's own climate suitability layer;
- whether to exclude BIOCLIM variables from range bagging model (only relevant if climate suitability file not specified);
- the probability of a pathway carrier containing the pest for each considered pathway;
- a likelihood penalty defined by distance from ports; and
- the scale at which outputs should be aggregated.


## Using `edmaps`

As highlighted in the above sections, `edmaps` requires multiple software and *R* packages to be installed. 
While these can be manually installed, we advise that this is done as a last resort. 
This is because each computer is different. 
Operating systems and software versions are likely to vary substantially between computers. 
As such it is extremely difficult ensure `edmaps` will run in all settings.

For this reason, and to ensure `edmaps` functionality into perpetuity, we strongly recommend that `edmaps` is implemented in a [*docker*](https://www.docker.com/) container. 
*Docker* is the world’s leading software container platform that is used to create lightweight, self-contained virtual Linux systems that contain all relevant open source software required to run developed software. 
Unlike other virtual machines, _Docker_ does not bundle a full operating system. 
Rather it only installs libraries and settings required to make the developed software work. 
This means that *Docker* can be used to eliminate the “it works on my machine” problems when running software. 
The other major advantage is that it removes the need for users to install software dependencies, as the hard work is already done.

In the following sections we outline three methods for running `edmaps`, given that `parameters.xlsx` has been filled out. While we recommend the *Docker* approach, we acknowledge that not all users will have the appropriate permissions to do so. 
As such we also outline two additional approaches for implementation: 
one that assumes software dependencies are correctly installed, but handles *R* package dependencies (`renv` approach), 
and one that assumes all dependencies are installed, and focuses on installation and use of `edmaps`.

## Using `edmaps` with Docker (recommended)
We have created a *Docker* image that contains all the system libraries, software (e.g. *R*, *Java*, *pandoc*) and *R* packages 
required to install and run workflows produced by `edmaps`.

To use the image, first install [Docker](https://docs.docker.com/get-started/) onto your machine. 
When installing *Docker* on *Windows*, you will be prompted to select whether to use Linux or *Windows* containers. 
Leave this at it's default (i.e. to use Linux containers). Once installed, we recommend users set *Docker* settings 
such that containers have access to at least 10 GB of RAM, and a specified number of CPUs (We recommend that users allow access up to all but one core for processing workflows derived from `edmaps`, especially if being used for many pests).

Next we can download an `edmaps` virtual machine by entering the following into the command line using Command Prompt or the terminal (requires internet connection):

```
docker pull jscamac/edmaps
```

Once the *Docker* image has been successfully downloaded, use the Command Prompt or terminal to navigate to the local copy of the data directory outlined above. 
Next run the system-specific command line:

**macOS & Linux**

```
docker run -d -v $(pwd):/home/ jscamac/edmaps R CMD BATCH --vanilla make.R make.log
```

**Windows**

```
docker run -d -v %cd%:/home/ jscamac/edmaps R CMD BATCH --vanilla make.R make.log
```

The above command line will launch the virtual machine and run `make.R` in non-interactive mode. 
This means that you may close the terminal or shell window and the virtual machine will continue running 
until all tasks have been completed upon which it will automatically shut down. 
The status and any error messages (if encountered) associated with the workflow progression can be examined by opening the log file `make.log`. 
This file will be saved in the data directory, alongside `make.R`. 
Once completed, all static and interactive maps as well as GIS-compatible rasters (GeoTIFF) can be found within the `outputs/` directory.


## Using `edmaps` with `renv`


If insufficient privileges exist to use _Docker_, an alternative is to use a [`renv`](https://rstudio.github.io/renv/articles/renv.html) (**r**eproducible **env**ironments) workflow.
The `renv` *R* package reproduces a pre-defined package environment, ensuring that specified package versions are used. To do this it creates a private, isolated package library for an *R* project, 
and obtains defined versions of packages from defined repositories. 
Versions and repositories are specified in a "lock file" (`renv.lock`), and the lock file describing the versions
used for the case studies has been provided with this report. Since the `renv` package library is project-specific, 
it will not overwrite or interfere with package versions used in other projects.

The most important difference between the *Docker* approach described above, and *renv*, is that the latter manages *R* packages only; required system libraries and tools must be installed manually.

Assuming software dependencies (e.g. *Java*, *R*) are available, recreating the package environment with `renv` is straightforward.

First, navigate to the data directory (see above) using Command Prompt or the terminal. Then run:

```
R CMD BATCH --vanilla make_renv.R make.log
```

This will create the local package environment, and commence the process of rebuilding the output datasets. 
When this process is underway, the file `make.log` will be created and will contain a live log of progress. 
If any errors are encountered, the error messages will be logged to this file. As the build progresses, 
files will be created within various subdirectories (e.g. `outputs/` directory).

If you see an error message stating that *R* is not a recognized command, you may need to provide the full path to the *R* 
executable, for example: 

```
c:/Program Files/R/R-4.0.2/bin/R CMD BATCH --vanilla make_renv.R make.log
```

## Using `edmaps` by itself
We recommend that this mode of running `edmaps` is only used as a last resort, or if one can be sure that the appropriate versions of all software and *R* package dependencies have been correctly installed.

Assuming all dependencies have been correctly installed, one need only install `edmaps`. 
This can be done by using Command Prompt or the terminal to navigate to the data directory and running:

```
R CMD INSTALL  edmaps_1.4.0.tar.gz
```

Alternatively, the latest version of `edmaps` can be directly downloaded from GitHub by running the following in 
R (assuming `devtools` is installed:

```
devtools::install_github("jscamac/edmaps")
```

Once edmaps has been installed, a workflow can be initiated by running:

```
R CMD BATCH --vanilla make.R make.log
```

As with other approaches, a log file (`make.log`) will be saved to the data directory and can be interrogated to assess analysis progression or any errors that may have been encountered.


## Troubleshooting?

If you have the appropriate data directory and are having issues implementing `edmaps` feel free to create an issue here outlining your problem. I'll endevour to resolve it ASAP.
