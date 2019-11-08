context('aggregate_raster')

test_that('aggregate_raster() reproduces test data (tif in, odd agg factor)', {
  aggregate_raster(
    rast = system.file('testdata/continuous.tif', package='edmaps'),
    outfile = f <- tempfile(fileext='.tif'),
    aggregate_factor = 5,
    fun = sum
  )
  
  testthat::expect_identical(
    tools::md5sum(f)[[1]], 
    tools::md5sum(system.file('testdata/aggsum5.tif', package='edmaps'))[[1]])
})

test_that('aggregate_raster() reproduces test data (tif in, even agg factor)', {
  aggregate_raster(
    rast = system.file('testdata/continuous.tif', package='edmaps'),
    outfile = f <- tempfile(fileext='.tif'),
    aggregate_factor = 6,
    fun = sum
  )
  
  testthat::expect_identical(
    tools::md5sum(f)[[1]], 
    tools::md5sum(system.file('testdata/aggsum6.tif', package='edmaps'))[[1]])
})


test_that('aggregate_raster() reproduces test data (raster in)', {
  aggregate_raster(
    rast = raster(system.file('testdata/continuous.tif', package='edmaps')),
    outfile = f <- tempfile(fileext='.tif'),
    aggregate_factor = 5,
    fun = sum
  )
  
  testthat::expect_identical(
    tools::md5sum(f)[[1]], 
    tools::md5sum(system.file('testdata/aggsum5.tif', package='edmaps'))[[1]])
})
