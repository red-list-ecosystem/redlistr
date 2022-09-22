test_that("accepts different format", {
  set.seed(1)

  # SpatialPoints
  pts = cbind(1:5, 1:5)
  dimnames(pts)[[1]] = letters[1:5]
  df = data.frame(a = 1:5)
  row.names(df) = letters[5:1]
  sp_points <- SpatialPoints(pts)
  crs(sp_points) <- "+proj=utm +zone=55 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"

  expect_equal(getAOO(sp_points, 1), 5)

  #SpatialPointsDataFrame
  sp_points_df <- SpatialPointsDataFrame(pts, df, match.ID = TRUE)
  crs(sp_points_df) <- "+proj=utm +zone=55 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"

  expect_equal(getAOO(sp_points_df, 1), 5)

  #SpatialPolygons
  Sr1 <- Polygon(cbind(c(2,4,4,1,2),c(2,3,5,4,2)))
  Sr2 <- Polygon(cbind(c(5,4,2,5),c(2,3,2,2)))
  Sr3 <- Polygon(cbind(c(4,4,5,10,4),c(5,3,2,5,5)))
  Srs1 <- Polygons(list(Sr1), "s1")
  Srs2 <- Polygons(list(Sr2), "s2")
  sp_polygons <- SpatialPolygons(list(Srs1,Srs2), 1:2)
  crs(sp_polygons) <- "+proj=utm +zone=55 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"

  expect_equal(getAOO(sp_polygons, 1), 10)

  #SpatialPolygonsDataframe
  grd <- GridTopology(c(1,1), c(1,1), c(10,10))
  polys <- as(grd, "SpatialPolygons")
  centroids <- coordinates(polys)
  x <- centroids[,1]
  y <- centroids[,2]
  z <- 1.4 + 0.1*x + 0.2*y + 0.002*x*x
  sp_polygons_df <- SpatialPolygonsDataFrame(polys,
                                             data=data.frame(x=x, y=y, z=z,
                                                             row.names=row.names(polys)))
  crs(sp_polygons_df) <- "+proj=utm +zone=55 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"

  expect_equal(getAOO(sp_polygons_df, 1), 100)

  #Raster
  r <- raster(nrows=10, ncols=10)
  values(r) <- 1
  crs(r) <- "+proj=utm +zone=55 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"

  expect_equal(getAOO(r, 100), 8)
})


test_that("minimum percent rule fails when using spatial points", {
  pts = cbind(1:5, 1:5)
  dimnames(pts)[[1]] = letters[1:5]
  df = data.frame(a = 1:5)
  row.names(df) = letters[5:1]
  sp_points <- SpatialPoints(pts)
  crs(sp_points) <- "+proj=utm +zone=55 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"

  expect_error(getAOO(sp_points, 1, T))
})
