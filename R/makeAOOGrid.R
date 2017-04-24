#' Create Area of Occupancy (AOO) grid for an ecosystem or species distribution
#'
#' \code{makeAOOGrid} creates grids for species presence based on the presented
#' raster object. It includes capability for specifying whether at least one
#' percent of the grid cell needs to be occupied before it is counted in the
#' AOO. This functionality is important for assessing the IUCN Red List of
#' Ecosystems Criteria B.
#'
#' @inheritParams createGrid
#' @param one.percent.rule Logical.If \code{TRUE} one percent of the grid cell
#'   must be occupied before it is counted in the AOO.
#' @return A shapefile of grid cells occupied by an ecosystem or species
#' @author Nicholas Murray \email{murr.nick@@gmail.com}, Calvin Lee
#' @seealso \code{\link{createGrid}}
#' @references Bland, L.M., Keith, D.A., Miller, R.M., Murray, N.J. and
#'   Rodriguez, J.P. (eds.) 2016. Guidelines for the application of IUCN Red
#'   List of Ecosystems Categories and Criteria, Version 1.0. Gland,
#'   Switzerland: IUCN. ix + 94pp. Available at the following web site:
#'   \url{iucnrle.org/}
#' @examples
#' n = createGrid(r1, 10000)
#' plot (n)
#' plot (r1, add = TRUE)
#' AOO_grid = makeAOOGrid(r1, n, one.percent.rule = F)
#' AOO_grid # shapefile of grid cells occupied by an ecosystem or species

makeAOOGrid <- function (ecosystem.data, grid.size, one.percent.rule = TRUE) {
  # Computes the number of 10x10km grid cells that are >1% covered by an ecosystem
  grid <- createGrid(ecosystem.data, grid.size)
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
  return (outGrid)
}

#test it
grid.size = 10000
n = makeAOOGrid(rast, grid.size, one.percent.rule = F)
o = makeAOOGrid(rast, grid.size, one.percent.rule = T)
plot(n, col = "darkgrey")
length(n)
plot(o, add = T,col = "red")
length(o)
# check in QGIS/ArcGIS
writeRaster(x, "papap.tif", format = "GTiff" )
shapefile(n, "q", overwrite=TRUE)
shapefile(o, "w", overwrite=TRUE)
