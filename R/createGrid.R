#'Create empty Area of Occupancy (AOO) Grid.
#'
#'Produces empty grid which can be used as the basis to help compute AOO.
#'
#'@param ecosystem.data Raster object of an ecosystem or species distribution
#'@param grid.size A number specifying the width of the desired grid square (in
#'  same units as your coordiante reference system)
#'@return A regular grid raster with extent \code{ecosystem.data} and grid size
#'  \code{grid.size}. Each grid square has a unique identification number.
#'@author Nicholas Murray \email{murr.nick@@gmail.com}, Calvin Lee
#'@seealso \code{\link{getAOO}}, \code{\link{makeAOOGrid}},
#'  \code{\link{getRLEReport}}
#'@references Bland, L.M., Keith, D.A., Miller, R.M., Murray, N.J. and
#'  Rodriguez, J.P. (eds.) 2016. Guidelines for the application of IUCN Red List
#'  of Ecosystems Categories and Criteria, Version 1.0. Gland, Switzerland:
#'  IUCN. ix + 94pp. Available at the following web site: \url{iucnrle.org/}
#' @examples
#' createGrid(raster1, 10000) # a 10-km grid with extent of raster1 with resolution
#' measured in m
#' createGrid(raster1, 2000) # a 2-km grid with extent of raster1 with resolution
#' measured in m
#' createGrid(raster1, 0.08983112) # a ~10-km grid with extent of raster with
#' resolution measured in degrees as 1-degree is ~111.32km at the equator

createGrid <- function(ecosystem.data, grid.size){
  grid <- raster::raster(ecosystem.data)
  res(grid) <- grid.size
  grid.expanded <- extend(grid, c(2,2)) # grow the grid by 2 each way
  grid.expanded[] <- 1:(ncell(grid.expanded))
  return (grid.expanded)
}
