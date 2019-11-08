context('rle_compress')

testthat::test_that('rle_compress replicates test file', {
  rle <- rle_compress(system.file('testdata/categorical.tif', package='edmaps'), 
                      f1 <- tempfile(fileext='.rds'), quiet=TRUE)
  testthat::expect_identical(
    tools::md5sum(f1)[[1]], 
    tools::md5sum(system.file('testdata/rle.rds', package='edmaps'))[[1]])
})
