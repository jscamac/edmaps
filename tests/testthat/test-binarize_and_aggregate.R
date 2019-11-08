context('binarize_and_aggregate')

test_that('binarize_and_aggregate() works with tif (no change to res or extent)', {
  binarize_and_aggregate(system.file('testdata/categorical.tif', package='edmaps'), 
                         outfile=f2 <- tempfile(fileext='.tif'),
                         categories=c(-2, 0, 1), quiet=TRUE)
  
  testthat::expect_identical(
    tools::md5sum(f2)[[1]], 
    tools::md5sum(system.file('testdata/binary.tif', package='edmaps'))[[1]])
})

test_that('binarize_and_aggregate() works with rle (rds) input', {
  binarize_and_aggregate(rle=system.file('testdata/rle.rds', package='edmaps'), 
                         outfile=f3 <- tempfile(fileext='.tif'),
                         categories=c(-2, 0, 1), quiet=TRUE)
  
  testthat::expect_identical(
    tools::md5sum(f3)[[1]], 
    tools::md5sum(system.file('testdata/binary.tif', package='edmaps'))[[1]])
})



test_that('binarize_and_aggregate() works when target res is a multiple of input res', {
  binarize_and_aggregate(rle=system.file('testdata/rle.rds', package='edmaps'), 
                         outfile=f4 <- tempfile(fileext='.tif'),
                         res=c(2, 2),
                         categories=c(-2, 0, 1), quiet=TRUE)
  
  testthat::expect_identical(
    tools::md5sum(f4)[[1]],
    tools::md5sum(system.file('testdata/binary_agg1.tif', package='edmaps'))[[1]])
})

test_that('binarize_and_aggregate() works when target res is tnot a multiple of input res', {
  binarize_and_aggregate(rle=system.file('testdata/rle.rds', package='edmaps'), 
                         outfile=f5 <- tempfile(fileext='.tif'),
                         res=c(2.5, 2.5),
                         categories=c(-2, 0, 1), quiet=TRUE)
  testthat::expect_identical(
    tools::md5sum(f5)[[1]],
    tools::md5sum(system.file('testdata/binary_agg2.tif', package='edmaps'))[[1]])
})

test_that('binarize_and_aggregate() works when target extent is aligned to raster', {
  binarize_and_aggregate(rle=system.file('testdata/rle.rds', package='edmaps'), 
                         outfile=f6 <- tempfile(fileext='.tif'),
                         res=c(2, 2), extent=c(-12, 26, -2, 90),
                         categories=c(-2, 0, 1), quiet=TRUE)
  
  testthat::expect_identical(
    tools::md5sum(f6)[[1]],
    tools::md5sum(system.file('testdata/binary_agg1_clip.tif', 
                              package='edmaps'))[[1]])
})


test_that('binarize_and_aggregate() works when target extent is not aligned to raster', {
  binarize_and_aggregate(rle=system.file('testdata/rle.rds', package='edmaps'), 
                         outfile=f7 <- tempfile(fileext='.tif'),
                         res=c(2, 2), extent=c(-12.5, 26, -2, 46.1),
                         categories=c(-2, 0, 1), quiet=TRUE)
  
  testthat::expect_identical(
    tools::md5sum(f7)[[1]],
    tools::md5sum(system.file('testdata/binary_agg1_clip2.tif', 
                              package='edmaps'))[[1]])
  
})
