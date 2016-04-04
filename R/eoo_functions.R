makeEOO <- function (ecosystem.data){
  # Makes an EOO spatial polygon using the centre point of each pixel as the boundary
  EOO.points <- rasterToPoints(ecosystem.data)
  if (nrow(EOO.points) <= 1) { # handling single pixels since chull fails for 1 pixel
    EOO.polygon <- rasterToPolygons(ecosystem.data)
  } else {
    EOO.chull <- chull(EOO.points)
    EOO.envelope <- EOO.points[EOO.chull,]
    c <- data.frame(EOO.envelope) # turn into a dataframe
    c[,3] <- NULL # get rid of the ID column ## FAILS HERE WHEN ECOSYSTEM IS 1 Pixel
    d <- as.matrix(c) #sp package needs a matrix
    e <- rbind(d, d[1,]) #this adds a row at the bottom to close the ring
    f <- Polygon(e) # creates a polygon object
    g <- Polygons(list(f), 1) # wrap it in a polygons object
    EOO.polygon <- SpatialPolygons(list(g)) # wrap it in a spatial polygons object
  }
  proj4string(EOO.polygon) <- crs(ecosystem.data)
  return(EOO.polygon)
}

E00.function <- function( r ) {
  # uses rgeos package
  ps <- rasterToPoints(r, spatial = TRUE)
  n = gConvexHull(ps)
  return (n)
}

getAreaEOO <- function(EOO.polygon){
  # Returns the area of the makeEOO output (spatialpolygons object)
  EOO.aream2 <- sapply(slot(EOO.polygon, "polygons"), slot, "area")  # get the area from the slots in the polygon dataset
  EOO.areakm2 <- EOO.aream2/1000000
  return(EOO.areakm2)
}
