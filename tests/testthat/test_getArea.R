context("Get Area")

test_that("accepts different input types", {
  r <- raster(nrows=10, ncols=10)
  r.bin <- r
  values(r.bin) <- 1

  r.multiple <- r
  values(r.multiple) <- rep(c(1:10), 10)

  expect_equal(getArea(r.bin), 0.1296)
  expect_warning(getArea(r.multiple), "The input raster is not binary, counting ALL non NA cells\n")
  expect_equal(getArea(r.multiple, 1), 0.01296)
})
