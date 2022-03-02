FROM rocker/geospatial:4.1.2

# Install dependencies
RUN install2.r --skipinstalled TRUE \
    countrycode \
    CoordinateCleaner \
    drake \
    fasterize \
    furrr \
    gdalUtilities \
    geometry \
    glue \
    htmlwidgets \
    leaflet \
    leaflet.opacity \
    leafem \
    mapedit \
    rasterVis \
    rgbif \
    rnaturalearth \
    viridis \
    testthat \
    future.callr \
    rnaturalearthdata \
    styler \
    OpenStreetMap \
    rnaturalearthhires \
    future.apply

## Install hi res geospatial data
RUN installGithub.r \
    ropenscilabs/rnaturalearthhires

## Install edmaps
RUN installGithub.r \
    jscamac/edmaps

# Set working directory
WORKDIR /home/Project
