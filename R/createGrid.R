#' Create Area of Occupancy (AOO) Grid.
#'
#' @param x Raster object of an ecosystem or species distribution
#' @param grid.size A number specifying the width of the desired grid square
#' (in metres)
#' @return A regular grid raster with extent \code{ecosystem.data} and grid size
#'   \code{grid.size}. Each grid square has a unique identification number
#' @author Nicholas Murray \email{murr.nick@@gmail.com}
#' @seealso \code{\link{getAOO}}, \code{\link{getRLEReport}}
#' @references Bland, L.M., Keith, D.A., Miller, R.M., Murray, N.J. and
#' Rodríguez, J.P. (eds.) 2016. Guidelines for the application of IUCN Red
#' List of Ecosystems Categories and Criteria, Version 1.0. Gland, Switzerland:
#' IUCN. ix + 94pp. Available at the following web site: \url{iucnrle.org/}
#' @examples
#' createGrid(raster1, 10000) # a 10-km grid with extent of raster1
#' createGrid(raster1, 2000) # a 2-km grid with extent of raster1

createGrid <- function(x, grid.size){
  grid <- raster(x)
  res(grid) <- grid.size
  grid[] <- 1:(ncell(grid))
  return (grid)
}
