FROM rocker/tidyverse:3.6.0
LABEL maintainer="James Camac"
LABEL email="james.camac@gmail.com"

RUN apt-get update && apt-get install -y --no-install-recommends \
    libudunits2-dev \
    libgeos-dev \
    libgdal-dev \
    gdal-bin \
    default-jdk \
    liblzma-dev \
    libbz2-dev  \
    libicu-dev  \
    libpcre3-dev \
    libz-dev \
    wget

WORKDIR /tmp/gdal
RUN wget http://download.osgeo.org/gdal/2.4.2/gdal-2.4.2.tar.gz \
 && tar zxf gdal-2.4.2.tar.gz \
 && cd gdal-2.4.2 \
 && ./configure \
 && make \
 && make install \
 && ldconfig \
 && rm -r /tmp/gdal

## Copies your description file into the Docker Container, specifying dependencies
COPY DESCRIPTION ./
# The above line adds only the description file for the project
RUN R -e "remotes::install_deps(upgrade='never', repos='http://cran.microsoft.com/snapshot/2019-09-30/')"

## Install other required packages.
RUN R -e "install.packages(c('OpenStreetMap', 'testthat', 'future.callr', 'rnaturalearthdata', 'lubridate', 'styler'), repos='http://cran.microsoft.com/snapshot/2019-09-30/')"

## Copy and install edmaps
COPY edmaps_1.0.0.tar.gz ./
RUN R CMD INSTALL edmaps_1.0.0.tar.gz

WORKDIR /home








