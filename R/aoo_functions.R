
#' Create an area of occupancy (AOO) grid.
#' 
#' @param ecosystem.data A raster dataset of an ecosystem or species 
#' distribution 
#' @param grid.size A number specifying the dimension of a single axis of the 
#' grid square (in metres)
#' @return A regular grid raster with extent \code{ecosystem.data} and grid size 
#' \code{grid.size}. Each grid square has a unique identification number
#' @examples
#' add(raster1, 10000) # a 10-km grid with extent of raster1
#' add(raster1, 2000) # a 2-km grid with extent of raster1

createGrid <- function(ecosystem.data, grid.size = 10000){
  grid <- raster(ecosystem.data)
  res(grid) <- grid.size
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
