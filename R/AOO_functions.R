#' Create empty Area of Occupancy (AOO) Grid.
#'
#' \code{createGrid} produces empty grid which can be used as the basis to help
#' compute AOO.
#'
#' @param ecosystem.data Raster object of an ecosystem or species distribution.
#'   Please use a CRS with units measured in metres.
#' @param grid.size A number specifying the width of the desired grid square (in
#'   same units as your coordiante reference system)
#' @return A regular grid raster with extent \code{ecosystem.data} and grid size
#'   \code{grid.size}. Each grid square has a unique identification number.
#' @author Nicholas Murray \email{murr.nick@@gmail.com}, Calvin Lee
#'   \email{calvinkflee@@gmail.com}
#' @family AOO functions
#' @seealso \code{\link{getRLEReport}}
#' @references Bland, L.M., Keith, D.A., Miller, R.M., Murray, N.J. and
#'   Rodriguez, J.P. (eds.) 2016. Guidelines for the application of IUCN Red
#'   List of Ecosystems Categories and Criteria, Version 1.0. Gland,
#'   Switzerland: IUCN. ix + 94pp. Available at the following web site:
#'   \url{iucnrle.org/}
#' @examples
#' createGrid(raster1, 10000) # a 10-km grid with extent of raster1 with resolution
#' createGrid(raster1, 2000) # a 2-km grid with extent of raster1 with resolution



createGrid <- function(ecosystem.data, grid.size){
  grid <- raster(ecosystem.data)
  res(grid) <- grid.size
  grid.expanded <- extend(grid, c(2,2)) # grow the grid by 2 each way
  grid.expanded[] <- 1:(ncell(grid.expanded))
  return (grid.expanded)
}

#' Create Area of Occupancy (AOO) grid for an ecosystem or species distribution
#'
#' \code{makeAOOGrid} creates grids for species presence based on the presented
#' raster object. It includes capability for specifying whether a minimum
#' percent of the grid cell needs to be occupied before it is counted in the
#' AOO. This functionality is important for assessing the IUCN Red List of
#' Ecosystems Criteria B.
#'
#' @inheritParams createGrid
#' @param min.percent.rule Logical. If \code{TRUE}, a minimum area threshold
#'   must be passed before a grid is counted as an AOO grid.
#' @param percent Numeric. The minimum percent to be applied as a threshold for
#'   the \code{min.percent.rule}
#' @return A shapefile of grid cells occupied by an ecosystem or species
#' @author Nicholas Murray \email{murr.nick@@gmail.com}, Calvin Lee
#'   \email{calvinkflee@@gmail.com}
#' @family AOO functions
#' @references Bland, L.M., Keith, D.A., Miller, R.M., Murray, N.J. and
#'   Rodriguez, J.P. (eds.) 2016. Guidelines for the application of IUCN Red
#'   List of Ecosystems Categories and Criteria, Version 1.0. Gland,
#'   Switzerland: IUCN. ix + 94pp. Available at the following web site:
#'   \url{iucnrle.org/}
#' @examples
#' n <- createGrid(r1, 10000)
#' plot (n)
#' plot (r1, add = TRUE)
#' AOO_grid <- makeAOOGrid(r1, n, one.percent.rule = F)
#' AOO_grid # shapefile of grid cells occupied by an ecosystem or species

makeAOOGrid <- function (ecosystem.data, grid.size, min.percent.rule = TRUE, percent = 1) {
  # Computes the number of 10x10km grid cells that are >1% covered by an ecosystem
  grid <- createGrid(ecosystem.data, grid.size)
  eco.points <- rasterToPoints(ecosystem.data)
  xy <- as.matrix(eco.points)[,c(1,2)] # select xy column only
  x <- rasterize(xy, grid, fun='count') # returns a 10 * 10 raster where cell value is the number of points in the cell
  names(x) <- 'count'
  grid.shp <- rasterToPolygons(x, dissolve=FALSE)
  if (min.percent.rule == FALSE){
    outGrid <- grid.shp
  }
  if (min.percent.rule == TRUE){
    cell.res <- res(ecosystem.data)
    area <- cell.res[1] * cell.res[2]
    one.pc.grid <- grid.size * grid.size / 100 # 1pc of grid cell
    threshold <- one.pc.grid * percent / area
    outGrid <- grid.shp[grid.shp$count > threshold,] # select only grids that meet one percent threshol
  }
  return (outGrid)
}

#' Compute Area of Occupancy (AOO)
#'
#' \code{getAOO} determines the number of area of occupancy (AOO) grid cells
#' occupied by a species or ecosystem. It includes capability for specifying
#' whether at least one percent of the grid cell needs to be occupied before it
#' is counted in the AOO. This functionality is important for assessing the IUCN
#' Red List of Ecosystems Criteria B.
#'
#' @inheritParams makeAOOGrid
#' @return The number of grid cells occupied by the ecosystem or species
#' @author Nicholas Murray \email{murr.nick@@gmail.com}, Calvin Lee
#'   \email{calvinkflee@@gmail.com}
#' @family AOO functions
#' @references Bland, L.M., Keith, D.A., Miller, R.M., Murray, N.J. and
#'   Rodriguez, J.P. (eds.) 2016. Guidelines for the application of IUCN Red
#'   List of Ecosystems Categories and Criteria, Version 1.0. Gland,
#'   Switzerland: IUCN. ix + 94pp. Available at the following web site:
#'   \url{iucnrle.org/}
#' @examples
#' n <- createGrid(r1, 10000)
#' plot (n)
#' plot (r1, add = TRUE)
#' AOO <- getAOO(r1, n, one.percent.rule = F)
#' AOO # number of grid cells occupied by an ecosystem or species

getAOO <- function (ecosystem.data, grid.size, min.percent.rule = TRUE, percent = 1){
  # Computes the number of 10x10km grid cells that are >1% covered by an ecosystem
  AOO.number = length(makeAOOGrid(ecosystem.data, grid.size, min.percent.rule, percent))
  return(AOO.number)
}

#' Alternate function for getting AOO (with custom grid)
#'
#' \code{getAOOSilent} is identical to \code{getAOO}, but allows the custom
#' input of the grid parameter. Used for \code{gridUncertainty}.
#' @param ecosystem.data Raster object of an ecosystem or species distribution.
#'   Please use a CRS with units measured in metres.
#' @param grid Custom grid to be used to calculate AOO. Usually the output of
#'   \code{gridUncertainty}
#' @param one.percent.rule Logical.If \code{TRUE} one percent of the grid cell
#'   must be occupied before it is counted in the AOO.
#' @author Nicholas Murray \email{murr.nick@@gmail.com}, Calvin Lee
#'   \email{calvinkflee@@gmail.com}
#' @family AOO functions

getAOOSilent <- function (ecosystem.data, grid, min.percent.rule = TRUE, percent = 1) {
  # Computes the number of 10x10km grid cells that are >1% covered by an ecosystem
  grid <- grid # below is different from getAOO
  grid.size <- res(grid)
  # here to end of if is the same as getAOO
  eco.points <- rasterToPoints(ecosystem.data)
  xy <- as.matrix(eco.points)[,c(1,2)] # select xy column only
  x <- rasterize(xy, grid, fun='count') # returns a 10 * 10 raster where cell value is the number of points in the cell
  names(x) <- 'count'
  grid.shp <- rasterToPolygons(x, dissolve=FALSE)
  if (min.percent.rule == FALSE){
    outGrid <- grid.shp
  }
  if (min.percent.rule == TRUE){
    cell.res <- res(ecosystem.data)
    area <- cell.res[1] * cell.res[2]
    one.pc.grid <- grid.size[1] * grid.size[2] / 100 # 1pc of grid cell
    threshold <- one.pc.grid * percent / area
    outGrid <- grid.shp[grid.shp$count > threshold,] # select only grids that meet one percent threshol
  }
  # end getAOO

  AOO.number = length(outGrid) ## different from getAOO

  return (AOO.number)
}
