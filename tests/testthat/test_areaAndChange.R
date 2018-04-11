context("Get Area")

test_that("input cannot be in lonlat crs",{
  r.crs <- raster(nrows=10, ncols=10)
  expect_error(getArea(r.crs), "Input raster has a longitude/latitude CRS.\nPlease reproject to a projected coordinate system")
})

test_that("accepts different input types", {
  # Dummy rasters for testing
  r <- raster(nrows=10, ncols=10)
  crs(r) <- "+proj=utm +zone=55 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
  r.bin <- r
  values(r.bin) <- 1
  r.multiple <- r
  values(r.multiple) <- rep(c(1:10), 10)

  expect_equal(getArea(r.bin), 0.1296)
  expect_warning(getArea(r.multiple), "The input raster is not binary, counting ALL non NA cells\n")
  expect_equal(getArea(r.multiple, 1), 0.01296)
})

context("Change functions")

test_that("decline stats work", {
  # Dummy areas and years
  A.t1 <- 100
  A.t2 <- 50
  year.t1 <- 2010
  year.t2 <- 2015
  dummy.decline.df <- getDeclineStats(A.t1, A.t2, year.t1, year.t2,
                                      methods = c('ARD', 'PRD', 'ARC'))
  expect_equal(dummy.decline.df$ARD, 10)
  expect_equal(dummy.decline.df$PRD, 12.94494, tolerance=1e-5)
  expect_equal(dummy.decline.df$ARC, -13.86294, tolerance=1e-5)
  expect_error(getDeclineStats(A.t1, A.t2, year.t1, year.t2))
})
