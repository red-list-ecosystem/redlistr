test_that("input cannot be in lonlat crs",{
  r.crs <- rast(nrows=10, ncols=10)
  expect_error(getArea(r.crs), "Input has a longitude/latitude CRS.\nPlease reproject to a projected coordinate system")
})

test_that("accepts different input rasters", {
  # Dummy rasters for testing
  r <- rast(nrows=10, ncols=10)
  crs(r) <- "epsg:32755"
  r.bin <- r
  values(r.bin) <- 1
  r.multiple <- r
  values(r.multiple) <- rep(c(1:10), 10)

  expect_equal(getArea(r.bin)$area, 0.06445729)
  expect_equal(nrow(getArea(r.multiple)), 10)
  expect_equal(getArea(r.multiple)$area[1], getArea(r.multiple)$area[2])
})

test_that("output is a single value", {
  # Dummy raster
  r <- rast(nrows=10, ncols=10)
  crs(r) <- "epsg:32755"
  values(r) <- rep(c(1, NA), 50)

  expect_equal(nrow(getArea(r)), 1)
})

test_that("accepts sf POLYGONS", {
  # Dummy rectangle
  x1 = 0
  x2 = 1000
  y1 = 0
  y2 = 1000
  my_polygon = st_polygon(list(cbind(c(x1,x1,x2,x2,x1),c(y1,y2,y2,y1,y1))))
  my_polygons = st_sf(ID = "A", crs = 32755, geometry = st_sfc(my_polygon))

  expect_equal(getArea(my_polygons)$area_km2, 1)
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

test_that("extrapolated estimates are correct", {
  # Dummy areas and years
  A.t1 <- 100
  A.t2 <- 50
  year.t1 <- 2010
  year.t2 <- 2015
  dummy.decline.df <- getDeclineStats(A.t1, A.t2, year.t1, year.t2,
                                      methods = c('ARD', 'PRD', 'ARC'))
  dummy_extrapolate_df <- extrapolateEstimate(A.t1, year.t1, 5,
                                              ARD = dummy.decline.df$ARD,
                                              PRD = dummy.decline.df$PRD,
                                              ARC = dummy.decline.df$ARC)
  expect_equal(dummy_extrapolate_df$A.ARD.t3, A.t2)
  expect_equal(dummy_extrapolate_df$A.PRD.t3, A.t2)
  expect_equal(dummy_extrapolate_df$A.ARC.t3, A.t2)
})
