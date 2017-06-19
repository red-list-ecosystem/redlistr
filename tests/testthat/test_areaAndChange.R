context("Get Area")

test_that("accepts different input types", {
  # Dummy rasters for testing
  r <- raster(nrows=10, ncols=10)
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
  dummy.decline.df <- getDeclineStats(A.t1, A.t2, year.t1, year.t2)

  expect_equal(dummy.decline.df$ARD, 10)
  expect_equal(dummy.decline.df$PRD, 12.94494, tolerance=1e-5)
  expect_equal(dummy.decline.df$ARC, -0.1386294, tolerance=1e-5)
})
