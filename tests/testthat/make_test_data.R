# Test data
set.seed(1)

r <- raster::setValues(raster::raster(), sample(-3:3, 180*360, replace=TRUE))
raster::writeRaster(r, 'inst/testdata/categorical.tif', datatype='INT2S', 
                    overwrite=TRUE)

rle_compress('inst/testdata/categorical.tif', 'inst/testdata/rle.rds',
             quiet=TRUE)

# no change to res or extent
binarize_and_aggregate('inst/testdata/categorical.tif',
                       outfile='inst/testdata/binary.tif',
                       categories=c(-2, 0, 1), quiet=TRUE)

# target res a multiple of input res
binarize_and_aggregate('inst/testdata/categorical.tif',
                       outfile='inst/testdata/binary_agg1.tif',
                       res=c(2, 2),
                       categories=c(-2, 0, 1), quiet=TRUE)

# target res not a multiple of input res
binarize_and_aggregate('inst/testdata/categorical.tif',
                       outfile='inst/testdata/binary_agg2.tif',
                       res=c(2.5, 2.5),
                       categories=c(-2, 0, 1), quiet=TRUE)

# target extent aligned to raster
binarize_and_aggregate('inst/testdata/categorical.tif',
                       outfile='inst/testdata/binary_agg1_clip.tif',
                       res=c(2, 2), extent=c(-12, 26, -2, 90),
                       categories=c(-2, 0, 1), quiet=TRUE)

# target extent not aligned to raster
binarize_and_aggregate('inst/testdata/categorical.tif',
                       outfile='inst/testdata/binary_agg1_clip2.tif',
                       res=c(2, 2), extent=c(-12.5, 26, -2, 46.1),
                       categories=c(-2, 0, 1), quiet=TRUE)


r2 <- raster::setValues(raster::raster(), runif(180*360))
raster::writeRaster(r2, 'inst/testdata/continuous.tif', overwrite=TRUE)

aggregate_raster('inst/testdata/continuous.tif', 
                 'inst/testdata/aggsum5.tif', 5, fun=sum)
aggregate_raster('inst/testdata/continuous.tif', 
                 'inst/testdata/aggsum6.tif', 6, fun=sum)



