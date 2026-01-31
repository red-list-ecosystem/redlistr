test_that("accepts different format", {
  set.seed(1)

  # sf POINTS
  pts = cbind(1:5, 1:5)
  dimnames(pts)[[1]] = letters[1:5]
  pts = data.frame(pts)
  sp_points <- st_as_sf(pts, coords = c("X1", "X2"), crs = 32755)

  expect_equal(getAOO(sp_points, 1)@AOO, 5)

  #sf POLYGONS
  poly <- list(
    st_polygon(list(cbind(c(2,4,4,1,2),c(2,3,5,4,2)))),
    st_polygon(list(cbind(c(5,4,2,5),c(2,3,2,2)))),
    st_polygon(list(cbind(c(4,4,5,10,4),c(5,3,2,5,5)))))

  poly_sf <- st_sf(id = 1:length(poly), geometry = st_sfc(poly), crs = 32755)

  expect_equal(getAOO(poly_sf, 1, jitter = F)@AOO, 21)

  #Raster
  r <- rast(nrows=10, ncols=10, crs = "epsg:32755")
  values(r) <- 1


  expect_equal(getAOO(r, 100)@AOO, 8)
})


test_that("minimum percent rule fails when using geocentric coordinates", {
  pts = cbind(1:5, 1:5)
  dimnames(pts)[[1]] = letters[1:5]
  pts = data.frame(pts)
  sp_points <- st_as_sf(pts, coords = c("X1", "X2"), crs = 4326)

  expect_error(getAOO(sp_points, 1))
})
