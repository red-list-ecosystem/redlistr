#' Create empty Area of Occupancy (AOO) Grid.
#'
#' `createGrid` produces empty grid which can be used as the basis to help
#' compute AOO.
#'
#' @param input.data Spatial object of an ecosystem or species distribution.
#'   Please use a CRS with units measured in metres.
#' @param grid.size A number specifying the width of the desired grid square (in
#'   same units as your coordinate reference system)
#' @return A regular grid raster with extent `input.data` and grid size
#'   `grid.size`. Each grid square has a unique identification number.
#' @author Nicholas Murray \email{murr.nick@@gmail.com}, Calvin Lee
#'   \email{calvinkflee@@gmail.com}
#' @family AOO functions
#' @references Bland, L.M., Keith, D.A., Miller, R.M., Murray, N.J. and
#'   Rodriguez, J.P. (eds.) 2016. Guidelines for the application of IUCN Red
#'   List of Ecosystems Categories and Criteria, Version 1.0. Gland,
#'   Switzerland: IUCN. ix + 94pp. Available at the following web site:
#'   <https://iucnrle.org/>
#' @import raster

createGrid <- function(input.data, grid.size){
  grid <- raster(extent(input.data))
  res(grid) <- grid.size
  grid.expanded <- extend(grid, c(2,2)) # grow the grid by 2 each way
  grid.expanded[] <- 1:(ncell(grid.expanded))
  return (grid.expanded)
}


#' Create Area of Occupancy (AOO) grid for an ecosystem or species distribution
#'
#' `makeAOOGrid` is a generic function that creates grids representing the
#' area of occupancy for distributions based on the input spatial data. It
#' includes capability for specifying whether a minimum percent of the grid cell
#' needs to be occupied before it is counted in the AOO. This functionality is
#' important for assessing the IUCN Red List of Ecosystems Criteria B.
#'
#' @inheritParams createGrid
#' @param min.percent.rule Logical. If `TRUE`, a minimum area threshold
#'   must be passed before a grid is counted as an AOO grid.
#' @param percent Numeric. The minimum percent to be applied as a threshold for
#'   the `min.percent.rule`
#' @return A shapefile of grid cells occupied by an ecosystem or species
#' @author Nicholas Murray \email{murr.nick@@gmail.com}, Calvin Lee
#'   \email{calvinkflee@@gmail.com}
#' @family AOO functions
#' @references Bland, L.M., Keith, D.A., Miller, R.M., Murray, N.J. and
#'   Rodriguez, J.P. (eds.) 2016. Guidelines for the application of IUCN Red
#'   List of Ecosystems Categories and Criteria, Version 1.0. Gland,
#'   Switzerland: IUCN. ix + 94pp. Available at the following web site:
#'   <https://iucnrle.org/>
#' @examples
#' crs.UTM55S <- '+proj=utm +zone=55 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs'
#' r1 <- raster(ifelse((volcano<130), NA, 1), crs = crs.UTM55S)
#' extent(r1) <- extent(0, 6100, 0, 8700)
#' AOO_grid <- makeAOOGrid(r1, 1000, min.percent.rule = TRUE, percent = 1)
#' @export
#' @import raster

makeAOOGrid <- function(input.data, grid.size, min.percent.rule = FALSE, percent = 1) {
  UseMethod("makeAOOGrid", input.data)
}

#' @export
makeAOOGrid.RasterLayer <-
  function(input.data, grid.size, min.percent.rule = FALSE, percent = 1) {
    grid <- createGrid(input.data, grid.size)
    input.points <- rasterToPoints(input.data)
    xy <- as.matrix(input.points)[,c(1,2)] # select xy column only
    x <- rasterize(xy, grid, fun='count') # returns a 10 * 10 raster where cell value is the number of points in the cell
    names(x) <- 'count'
    grid.shp <- rasterToPolygons(x, dissolve=FALSE)
    if (min.percent.rule == FALSE){
      outGrid <- grid.shp
    }
    if (min.percent.rule == TRUE){
      cell.res <- res(input.data)
      area <- cell.res[1] * cell.res[2]
      one.pc.grid <- grid.size * grid.size / 100 # 1pc of grid cell
      threshold <- one.pc.grid * percent / area
      outGrid <- grid.shp[grid.shp$count > threshold, ] # select only grids that meet one percent threshol
    }
    return (outGrid)
  }

#' @export
makeAOOGrid.SpatialPoints <-
  function(input.data, grid.size, min.percent.rule = FALSE, percent = 1){
    if (min.percent.rule == T) {
      stop("min.percent.rule cannot be used when input is SpatialPoints as
           points do not have an inherent area. Consider converting into another
           format to use this function")
    }
    grid <- createGrid(input.data, grid.size)
    xy <- input.data@coords
    x <- rasterize(xy, grid, fun='count') # returns a 10 * 10 raster where cell value is the number of points in the cell
    names(x) <- 'count'
    grid.shp <- rasterToPolygons(x, dissolve=FALSE)
    outGrid <- grid.shp
    return (outGrid)
  }

#' @export
makeAOOGrid.SpatialPolygons <-
  function(input.data, grid.size, min.percent.rule = FALSE, percent = 1){
    grid <- createGrid(input.data, grid.size)
    x <- rasterize(input.data, grid, getCover = T)
    names(x) <- 'cover'
    grid.shp <- rasterToPolygons(x, dissolve = F)
    if (min.percent.rule == FALSE){
      outGrid <- grid.shp[grid.shp$cover > 0, ]
    }
    if (min.percent.rule == TRUE){
      outGrid <- grid.shp[grid.shp$cover > (percent / 100), ]
    }
    return(outGrid)
  }

#' Compute Area of Occupancy (AOO)
#'
#' `getAOO` determines the number of area of occupancy (AOO) grid cells
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
#'   <https://iucnrle.org/>
#' @examples
#' crs.UTM55S <- '+proj=utm +zone=55 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs'
#' r1 <- raster(ifelse((volcano<130), NA, 1), crs = crs.UTM55S)
#' extent(r1) <- extent(0, 6100, 0, 8700)
#' AOO <- getAOO(r1, 1000, min.percent.rule = TRUE, percent = 1)
#' @export

getAOO <- function(input.data, grid.size, min.percent.rule = FALSE, percent = 1){
  # Computes the number of 10x10km grid cells that are >1% covered by an ecosystem
  AOO.number <- length(makeAOOGrid(input.data, grid.size, min.percent.rule, percent))
  return(AOO.number)
}

#' Alternate function for getting AOO (with custom grid)
#'
#' `getAOOSilent` is identical to `getAOO`, but allows the custom
#' input of the grid parameter. Used for `gridUncertainty`.
#' @param input.data Spatial object of an ecosystem or species distribution.
#'   Please use a CRS with units measured in metres.
#' @param grid Custom grid to be used to calculate AOO. Usually the output of
#'   `gridUncertainty`
#' @param min.percent.rule Logical. If `TRUE` one percent of the grid cell
#'   must be occupied before it is counted in the AOO.
#' @param percent Numeric. The minimum percent to be applied as a threshold for
#'   the `min.percent.rule`
#' @return Value. The AOO calculated with the input distribution and grid.
#' @author Nicholas Murray \email{murr.nick@@gmail.com}, Calvin Lee
#'   \email{calvinkflee@@gmail.com}
#' @family AOO functions
#' @import raster

getAOOSilent <- function(input.data, grid, min.percent.rule = FALSE, percent = 1) {
  UseMethod("getAOOSilent", input.data)
}

#' @export
getAOOSilent.RasterLayer <-
  function(input.data, grid, min.percent.rule = FALSE, percent = 1) {
    # Different from getAOO
    grid <- grid
    grid.size <- res(grid)

    # Same as getAOO
    input.points <- rasterToPoints(input.data)
    xy <- as.matrix(input.points)[,c(1,2)] # select xy column only
    x <- rasterize(xy, grid, fun='count') # returns a 10 * 10 raster where cell value is the number of points in the cell
    names(x) <- 'count'
    grid.shp <- rasterToPolygons(x, dissolve=FALSE)
    if (min.percent.rule == FALSE){
      outGrid <- grid.shp
    }
    if (min.percent.rule == TRUE){
      cell.res <- res(input.data)
      area <- cell.res[1] * cell.res[2]
      one.pc.grid <- grid.size[1] * grid.size[2] / 100 # 1pc of grid cell
      threshold <- one.pc.grid * percent / area
      outGrid <- grid.shp[grid.shp$count > threshold, ] # select only grids that meet one percent threshol
    }

    # end getAOO
    AOO.number <- length(outGrid)
    return(list(AOO.number = AOO.number,
                out.grid = outGrid))
  }

#' @export
getAOOSilent.SpatialPoints <-
  function(input.data, grid, min.percent.rule = FALSE, percent = 1){

    if (min.percent.rule == T) {
      stop("min.percent.rule cannot be used when input is SpatialPoints as
           points do not have an inherent area. Consider converting into another
           format to use this function")
    }

    # Different from getAOO
    grid <- grid
    grid.size <- res(grid)

    #Same as getAOO
    xy <- input.data@coords
    x <- rasterize(xy, grid, fun='count') # returns a 10 * 10 raster where cell value is the number of points in the cell
    names(x) <- 'count'
    grid.shp <- rasterToPolygons(x, dissolve=FALSE)
    outGrid <- grid.shp

    # end getAOO
    AOO.number <- length(outGrid)
    return(list(AOO.number = AOO.number,
                out.grid = outGrid))
  }

#' @export
getAOOSilent.SpatialPolygons <-
  function(input.data, grid, min.percent.rule = FALSE, percent = 1){
    # Different from getAOO
    grid <- grid
    grid.size <- res(grid)

    #Same as getAOO
    x <- rasterize(input.data, grid, getCover = T)
    names(x) <- 'count'
    grid.shp <- rasterToPolygons(x, dissolve = F)
    if (min.percent.rule == FALSE){
      outGrid <- grid.shp[grid.shp$count > 0, ]
    }
    if (min.percent.rule == TRUE){
      outGrid <- grid.shp[grid.shp$count > (percent / 100), ]
    }

    # end getAOO
    AOO.number <- length(outGrid)
    return(list(AOO.number = AOO.number,
                out.grid = outGrid))
  }
