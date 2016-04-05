
getAreaEOO <- function(EOO.polygon){
  # Returns the area of the makeEOO output (spatialpolygons object)
  EOO.aream2 <- sapply(slot(EOO.polygon, "polygons"), slot, "area")  # get the area from the slots in the polygon dataset
  EOO.areakm2 <- EOO.aream2/1000000
  return(EOO.areakm2)
}
