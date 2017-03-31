#' Silent function for grid uncertainty
#'
# only difference is you can pass a grid to getAOO
# used for grid uncertainty


getAOOSilent <- function (ecosystem.data, grid, one.percent.rule = FALSE) {
  # Computes the number of 10x10km grid cells that are >1% covered by an ecosystem
  grid <- grid # below is different from getAOO
  grid.size = res(grid)
  # here to end of if is the same as getAOO
  eco.points <- rasterToPoints(ecosystem.data)
  xy <- as.matrix(eco.points)[,c(1,2)] # select xy column only
  x <- rasterize(xy, grid, fun='count') # returns a 10 * 10 raster where cell value is the number of points in the cell
  names(x) <- 'count'
  grid.shp <- rasterToPolygons(x, dissolve=FALSE)
  if (one.percent.rule == FALSE){
    outGrid <- grid.shp
  }
  if (one.percent.rule == TRUE){
    cell.res <- res(ecosystem.data)
    area <- cell.res[1] * cell.res[2]
    one.pc.grid <- grid.size * grid.size / 100 # 1pc of grid cell
    threshold <- one.pc.grid / area
    outGrid <- grid.shp[grid.shp$count > threshold,] # select only grids that meet one percent threshol
  }
  # end getAOO

  AOO.number = length(outGrid) ## different from getAOO

  return (AOO.number)
}
