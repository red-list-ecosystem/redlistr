
createGrid <- function(ecosystem.data, resolution = 10000){
  # Make an Area of Occupancy (AOO) grid with the same extent as input raster file.
  # Soecufsize input (in m) and  output a raster with different gridID values for each grid square.
  grid <- raster(ecosystem.data)
  res(grid) <- resolution
  grid[] <- 1:(ncell(grid))
  return (grid)
}

getAOO <- function (ecosystem.data, grid, one.percent.rule = TRUE) {
  # Computes the number of 10x10km grid cells that are >1% covered by an ecosystem 
  agg.extent <- extent(ecosystem.data)
  agg.resample <- resample(ecosystem.data, grid, method ="ngb")
  zonal.stat <- zonal(agg.resample,grid, 'sum') # provides stats of number of grid cells in each AOO cell
  zonal.data <- as.data.frame(zonal.stat)
  cell.res <- res(ecosystem.data)
  zonal.data$area <-((cell.res[1]*cell.res[2])*zonal.data$sum)/1000000
  if (one.percent.rule == TRUE){
    zonal.data$AOO <- zonal.data$area>1 # >1km2 for 1pc AOO
    AOO.number <- sum(zonal.data$AOO)  
  }
  if (one.percent.rule == FALSE){
    zonal.data$AOO <- zonal.data$area # >1km2 for 1pc AOO
    AOO.number <- sum(zonal.data$AOO)  
  }
  return(AOO.number)
}

# n = createGrid(r1, resolution = 10000)
# plot (n)
# plot (r1, add = TRUE)
# getAOO(r1, n, one.percent.rule = F)
# grid
# head(grid)
# 
