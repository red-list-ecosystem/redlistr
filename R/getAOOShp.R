#' Compute Area of Occupancy (AOO)
#'
#' \code{getAOO} determines the number of area of occupancy (AOO) grid cells
#' occupied by a species or ecosystem. It includes capability for specifying
#' the proportion of each grid cell that needs to be occupied before the grid
#' cell is counted in the AOO. This functionality is important for assessing
#' the IUCN Red List of Ecosystems Criteria B.
#'
#' @param x Raster object of an ecosystem or species distribution
#' @param grid Raster object of a regular grid suitable for assessing AOO
#' @return The number of grid cells occupied by the ecosystem or species
#' @author Nicholas Murray \email{murr.nick@@gmail.com}
#' @seealso \code{\link{createGrid}}
#' @references Bland, L.M., Keith, D.A., Miller, R.M., Murray, N.J. and
#' Rodriguez, J.P. (eds.) 2016. Guidelines for the application of IUCN Red
#' List of Ecosystems Categories and Criteria, Version 1.0. Gland, Switzerland:
#' IUCN. ix + 94pp. Available at the following web site: \url{iucnrle.org/}
#' @examples
#' n = createGrid(r1, 10000)
#' plot (n)
#' plot (r1, add = TRUE)
#' AOO = getAOO(r1, n, one.percent.rule = F)
#' AOO # number of grid cells occupied by an ecosystem or species

getAOOShp <- function (ecosystem.data, grid.size, one.percent.rule = TRUE) {
  # Computes the number of 10x10km grid cells that are >1% covered by an ecosystem
  grid <- createGrid(ecosystem.data, grid.size)
  eco.points <- rasterToPoints(ecosystem.data)
  xy <- as.matrix(eco.points)[,c(1,2)] # select xy column only
  x <- rasterize(xy, grid, fun='count') # returns a 10 * 10 raster where cell value is the number of points in the cell
  NAvalue(x) <- 0
  zonal.data = as.data.frame(freq(x, useNA = 'no'))
  cell.res <- res(ecosystem.data)
  zonal.data$area <- ((cell.res[1]*cell.res[2])*zonal.data$value)/100000

  ## CHECK SHOULD IT BE 100000 or 1000000????

  if (one.percent.rule == TRUE){
    zonal.data$AOO <- zonal.data$area > 1 # >1km2 for 1pc AOO
    AOO.number <- sum(zonal.data$AOO)
  }
  if (one.percent.rule == FALSE){
    zonal.data$AOO <- zonal.data$area > 0 # >1km2 for 1pc AOO
    AOO.number <- sum(zonal.data$AOO)
  }
  return(AOO.number)
}


## NOTE THIS IS FASTER THAN THE OLD VERSION
