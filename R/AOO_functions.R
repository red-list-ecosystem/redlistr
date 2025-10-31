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
#'   \email{calvinkflee@@gmail.com}, Aniko B. Toth \email{anikobtoth@@gmail.com}
#' @family AOO functions
#' @references Bland, L.M., Keith, D.A., Miller, R.M., Murray, N.J. and
#'   Rodriguez, J.P. (eds.) 2016. Guidelines for the application of IUCN Red
#'   List of Ecosystems Categories and Criteria, Version 1.0. Gland,
#'   Switzerland: IUCN. ix + 94pp. Available at the following web site:
#'   <https://iucnrle.org/>
#' @import terra

createGrid <- function(input.data, grid.size = 10000){
  grid <- terra::rast(ext(input.data), res = grid.size, crs = crs(input.data))
  grid.expanded <- terra::extend(grid, c(2,2)) # grow the grid by 2 each way
  grid.expanded[] <- 1:(ncell(grid.expanded))  # number the cells
  return (grid.expanded)
}


#' Identify positions of the bottom 1% of ecosystem area in AOO grid
#'
#' `top_pct` returns the vector positions of the largest elements
#' collectively comprising a given percentage more of the vector sum. This function
#' helps perform the bottom.1pct.rule when selecting the AOO grid by
#' identifying grid positions to keep.
#'
#' @param v A numeric vector.
#' @param pct percent of area to drop
#' @author Aniko B. Toth \email{anikobtoth@@gmail.com}
#' @family AOO functions
#' @references IUCN (2024). Guidelines for the application of IUCN Red
#' List of Ecosystems Categories and Criteria, Version 2.0. Keith, D.A.,
#' Ferrer-Paris, J.R., Ghoraba, S.M.M., Henriksen, S., Monyeki, M., Murray,
#' N.J., Nicholson, E., Rowland, J., Skowno, A., Slingsby, J.A., Storeng,
#' A.B., Valderrábano, M. & Zager, I. (Eds.). Gland, Switzerland: IUCN.
#' ix + 94pp. Available at the following web site:
#'   <https://iucnrle.org/>

top_pct <- function(v, pct = 99) {
  target <- (pct/100)*sum(v)
  out <- sort(v)
  while(sum(out) > target) {out <- out[-1]}
  drop <- length(v) - (length(out) + 1)
  return(tail(order(v), (length(v) - drop)))
}

#' Create Area of Occupancy (AOO) grid for an ecosystem or species distribution
#'
#' `makeAOOGrid` is a generic function that creates grids representing the
#' area of occupancy for distributions based on the input spatial data. It
#' includes capability for specifying whether the least occupied cells collectively
#' containing less than 1% of the ecosystem are counted in the AOO. This functionality is
#' important for assessing the IUCN Red List of Ecosystems Criteria B.
#'
#' @inheritParams createGrid
#' @param bottom.1pct.rule Logical. If `TRUE`, grid cells containing the least
#' ecosystem area are dropped up to 1% of the total distribution.
#' @param percent Numeric. The minimum percent to be applied as a threshold for
#'   the `bottom.1pct.rule`
#' @return A shapefile of grid cells occupied by an ecosystem or species
#' @author Nicholas Murray \email{murr.nick@@gmail.com}, Calvin Lee
#'   \email{calvinkflee@@gmail.com}, Aniko B. Toth \email{anikobtoth@@gmail.com}
#' @family AOO functions
#' @references Bland, L.M., Keith, D.A., Miller, R.M., Murray, N.J. and
#'   Rodriguez, J.P. (eds.) 2016. Guidelines for the application of IUCN Red
#'   List of Ecosystems Categories and Criteria, Version 1.0. Gland,
#'   Switzerland: IUCN. ix + 94pp. Available at the following web site:
#'   <https://iucnrle.org/>
#' @examples
#' crs.UTM55S <- '+proj=utm +zone=55 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs'
#' r1 <- rast(ifelse((volcano<130), NA, 1), crs = crs.UTM55S)
#' extent(r1) <- extent(0, 6100, 0, 8700)
#' AOO_grid <- makeAOOGrid(r1, 1000, bottom.1pct.rule = TRUE, percent = 1)
#' @export
#' @import terra
#' @import sf
#' @import dplyr

makeAOOGrid <- function(input.data, grid.size = 10000, names_from = NA, bottom.1pct.rule = TRUE, percent = 1, jitter = TRUE) {
  UseMethod("makeAOOGrid", input.data)
}

#' @export
makeAOOGrid.SpatRaster <-
  function(input.data, grid.size, bottom.1pct.rule = TRUE, percent = 1, jitter = TRUE) {
    grid <- createGrid(input.data, grid.size)

    if(jitter){
      dx <- stats::runif(1, -1, 1) * grid.size/2
      dy <- stats::runif(1, -1, 1) * grid.size/2

      # Shift the extent
      ext_r <- ext(grid)
      ext_r_shifted <- ext(
        xmin(ext_r) + dx,
        xmax(ext_r) + dx,
        ymin(ext_r) + dy,
        ymax(ext_r) + dy
      )

      # Apply the shifted extent
      ext(grid) <- ext_r_shifted
    }  # randomly moves the grid around by half the grid size

    input.points <- terra::as.points(input.data)
    x <- split(input.points, names(input.points)) |>
      lapply(rasterizeGeom, grid, fun = "count") |> # returns 10 * 10 rasters where cell value is the number of points in the cell
      lapply(function(.x){
        names(.x) <- "count"
        .x
      })
    AOO_grid <- lapply(x, as.polygons, dissolve=FALSE) |> lapply(function(.x) .x[.x$count > 0,]) # remove grid cells with no instances of the ecosystem

    if(bottom.1pct.rule)
      AOO_grid <- lapply(AOO_grid, function(.x) .x[top_pct(.x$count, pct = 100-percent)])

    AOO_grid <- lapply(AOO_grid, st_as_sf)

   return(AOO_grid)
  }

#' @export
makeAOOGrid.sf <-
  function(input.data, grid.size = 10000, names_from = NA, bottom.1pct.rule = TRUE, percent = 1, jitter = TRUE) {

    names_from <- dplyr::coalesce(names_from, "ecosystem_name")

    # identify name field
    if (!any(colnames(input.data) %in% names_from)) {                 # check for ecosystem names
      input.data <- input.data |> dplyr::mutate(ecosystem_name = "unnamed ecosystem type")  #put new name label if not present
    }
    ecosystem_names <- dplyr::pull(input.data, !!names_from)  # pull ecosystem names

    # count number of ecosystem types
    if (dplyr::n_distinct(ecosystem_names) == 1) {
      #message("Only one ecosystem type entered, consider using `create_AOO_grid` to get more detailed summary.")
    }

    # create assessment grid
    grid <- createGrid(input.data, grid.size)
    if(jitter){
      dx <- stats::runif(1, -1, 1) * grid.size/2
      dy <- stats::runif(1, -1, 1) * grid.size/2

      # Shift the extent
      ext_r <- ext(grid)
      ext_r_shifted <- ext(
        xmin(ext_r) + dx,
        xmax(ext_r) + dx,
        ymin(ext_r) + dy,
        ymax(ext_r) + dy
      )

      # Apply the shifted extent
      ext(grid) <- ext_r_shifted
    }  # randomly moves the grid around by half the grid size

    # check geometries
    if (all(st_geometry_type(input.data) %in% c("POINT", "LINESTRING", "MULTIPOINT", "MULTILINESTRING", "POLYGON", "MULTIPOLYGON"))){

      if(all(st_geometry_type(input.data) %in% c("POINT", "MULTIPOINT"))){                ## Case where all inputs are points

          x <- input.data |> split(st_drop_geometry(input.data[names_from])) |>
            lapply(st_coordinates) |>
            lapply(rasterize, grid, fun='count') # returns raster where value is the number of points in cell
          AOO_grid <- lapply(x, as.polygons, dissolve=FALSE)
          if(bottom.1pct.rule == T) {
           warning("bottom.1pct.rule will not be accurate when input has POINTS geometry because
                 points do not have an inherent area. Consider converting into another
                 format. Rule has been applied based on point counts in each grid square.")
          AOO_grid <- lapply(AOO_grid, function(.x) .x[top_pct(.x$count, pct = 100-percent)])
          }
          return (AOO_grid |> lapply(st_as_sf))

        } else if (all(st_geometry_type(input.data) %in% c("LINESTRING", "MULTILINESTRING"))) {  ## case where all inputs are lines
          if (st_is_longlat(input.data)) { # check CRS
            warning("AOO is being calculated in a geographic coordinate reference system. Use st_project() to change to a planar CRS first!")
          }  # check CRS
          grid_poly <- as.polygons(grid, dissolve = FALSE) |> sf::st_as_sf()

          overlaps <- st_intersection(grid_poly, input.data)
          overlaps$length_m <- st_length(overlaps) |> as.numeric()

          AOO <- overlaps |> base::split(st_drop_geometry(overlaps[names_from]))

          if(bottom.1pct.rule){
            warning("bottom.1pct.rule is not accurate when input has LINESTRING geometry because
                 lines do not have an inherent area. Consider converting into another
                 format. Rule has been applied based on LINESTRING lengths in each grid square.")
            AOO <- lapply(AOO, function(.x) .x[top_pct(.x$length_m, pct = 100-percent),])  # remove bottom 1% of ecosystem area.
           }
          AOO_grid <- AOO |> lapply(function(.x) return(grid_poly |> dplyr::filter(lyr.1 %in% .x$lyr.1)))
          return(AOO_grid)


        } else if (all(st_geometry_type(input.data) %in% c("POLYGON", "MULTIPOLYGON"))) { ## case where all input geometries are POLYGON or MULTIPOLYGON

          if (st_is_longlat(input.data)) { # check CRS
            warning("AOO is being calculated in a geographic coordinate reference system. Use st_project() to change to a planar CRS first!")
        }  # check CRS
          grid_poly <- as.polygons(grid, dissolve = FALSE) |> sf::st_as_sf()

          overlaps <- st_intersection(grid_poly, input.data)
          overlaps$area_m2 <- st_area(overlaps) |> as.numeric()

          AOO <- overlaps |> base::split(st_drop_geometry(overlaps[names_from]))

          if(bottom.1pct.rule)
            AOO <- lapply(AOO, function(.x) .x[top_pct(.x$area_m2, pct = 100-percent),])  # remove bottom 1% of ecosystem area.

          AOO_grid <- AOO |> lapply(function(.x) return(grid_poly |> dplyr::filter(lyr.1 %in% .x$lyr.1)))
          return(AOO_grid)
      } else {
         stop("Multiple geometry types detected. Please enter input data with a consistent geometry")
      } # case where the input data has a mix of accepted geometries.

     } else {
      stop("Your input data contains geometries that are not supported. Please enter a dataset that has POLYGON, LINESTRING, or POINT geometry.")
     }  ## case where there are unusual geometries in the input data.
  }

makeAOOGrid.AOOgrid <-
  function(input.data){

    # flag NA values for computation purposes
   if (inherits(input.data@input, "SpatRaster")){
      ecosystemunit <- terra::deepcopy(input.data@input)
      terra::NAflag(ecosystemunit) <- 0
   }else if(inherits(input.data@input, "sf")){
     ecosystemunit <- input.data@input
   }
   grid.size <- input.data@params$gridsize

   n = 10
   output <- lapply(1:n, function(x) makeAOOGrid(ecosystemunit, grid.size)) |>
     lapply(`[[`, 1) #flatten list
   AOO_vals <- sapply(output, nrow)
   best_grid <- output[[which.min(AOO_vals)[1]]]

   output <-
     new("AOOgrid",
       grid = best_grid,
       AOO = nrow(best_grid),
       params = list(gridsize = grid.size, jitter = TRUE, pct = percent, n = n),
       pctrule = bottom.1pct.rule,
       input = input.data@input,
       AOOvals = AOO_vals)
   return(output)
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
#' @return an object of class AOOgrid or a list of AOOgrid objects. Ecosystems that recieved
#' an AOO of 60 cells or fewer on a first pass are run with a jittered grid of n=100
#' @author Nicholas Murray \email{murr.nick@@gmail.com}, Calvin Lee
#'   \email{calvinkflee@@gmail.com}, Aniko B. Toth \email{anikobtoth@@gmail.com}
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
#' AOO <- getAOO(r1, 1000, bottom.1pct.rule = TRUE, percent = 1)
#' @export
getAOO <-  function(input.data, grid.size = 10000, names_from = NA, bottom.1pct.rule = TRUE, percent = 1, jitter = TRUE) {
  UseMethod("getAOO", input.data)
}

#' @export
getAOO.SpatRaster <- function(input.data, grid.size = 10000, bottom.1pct.rule = TRUE, percent = 1, jitter = TRUE) {
  message("Initialising grids")
  AOO_grid <- makeAOOGrid(input.data, grid.size, bottom.1pct.rule, percent, jitter)

  message("Assembling initial grids")
  # Split raster into list of binary rasters
  binary_rasters <- lapply(sort(unique(terra::values(input.data))), function(v) {
    binary <- as.numeric(input.data == v)
    names(binary) <- paste0("value_", v)
    binary
  })

  AOOgrid_list <- lapply(1:length(AOO_grid),
                         function(x) new("AOOgrid",
                                         grid = AOO_grid[[x]],
                                         AOO = nrow(AOO_grid[[x]]),
                                         params = list(gridsize = grid.size, jitter = jitter, pct = percent),
                                         pctrule = bottom.1pct.rule,
                                         input = binary_rasters[[x]]))

  # run grid jitter on units with AOO near a threshold
  if(jitter){
    message("Running jitter on units with 60 or fewer cells")
    AOOgrid_list <- lapply(AOOgrid_list, function(x) if(x@AOO <= 60 & x@AOO > 2) {return(makeAOOGrid(x)) } else {return(x)})
    AOOgrid_list <- setNames(AOOgrid_list, names(AOO_grid))
    return(AOOgrid_list)
  }
}

#' @export
getAOO.sf <-  function(input.data, grid.size = 10000, names_from = NA, bottom.1pct.rule = TRUE, percent = 1, jitter = TRUE){
  message("Initialising grids")
  AOO_grid <- makeAOOGrid(input.data, grid.size, names_from, bottom.1pct.rule, percent, jitter)

  message("Assembling initial grids")
  names_from <- dplyr::coalesce(names_from, "ecosystem_name")
  if (!any(colnames(input.data) %in% names_from)) {                 # check for ecosystem names
    input.data <- input.data |> dplyr::mutate(ecosystem_name = "unnamed ecosystem type")  #put new name label if not present
  }
  input_split <- input.data |> split(st_drop_geometry(input.data[names_from]))

  AOOgrid_list <- lapply(1:length(AOO_grid),
                         function(x) new("AOOgrid",
                                         grid = AOO_grid[[x]],
                                         AOO = nrow(AOO_grid[[x]]),
                                         params = list(gridsize = grid.size, jitter = jitter, pct = percent),
                                         pctrule = bottom.1pct.rule,
                                         input = input_split[[x]]))

  # run grid jitter on units with AOO near a threshold
  if(jitter){
    message("Running jitter on units with 60 or fewer cells")
    AOOgrid_list <- lapply(AOOgrid_list, function(x) if(x@AOO <= 60 & x@AOO > 2) {return(makeAOOGrid(x)) } else {return(x)})
    AOOgrid_list <- setNames(AOOgrid_list, names(AOO_grid))
  }

  if(length(AOOgrid_list) == 1) { return(AOOgrid_list[[1]])
  } else {
      return(AOOgrid_list) }

}

#' Alternate function for getting AOO (with custom grid)
#'
#' `getAOOSilent` is identical to `getAOO`, but allows the custom
#' input of the grid parameter. Used for `gridUncertainty`.
#' @param input.data Spatial object of an ecosystem or species distribution.
#'   Please use a CRS with units measured in metres.
#' @param grid Custom grid to be used to calculate AOO. Usually the output of
#'   `gridUncertainty`
#' @param bottom.1pct.rule Logical. If `TRUE`, grid cells containing the least
#' ecosystem area are dropped up to 1% of the total distribution.
#' @param percent Numeric. The percent to be applied as a threshold for
#'   the `bottom.1pct.rule`
#' @return Value. The AOO calculated with the input distribution and grid.
#' @author Nicholas Murray \email{murr.nick@@gmail.com}, Calvin Lee
#'   \email{calvinkflee@@gmail.com}
#' @family AOO functions
#' @import raster

getAOOSilent <- function(input.data, grid, bottom.1pct.rule = TRUE, percent = 1) {
  UseMethod("getAOOSilent", input.data)
}

#' @export
getAOOSilent.RasterLayer <-
  function(input.data, grid, bottom.1pct.rule = TRUE, percent = 1) {
    # Different from getAOO
    grid <- grid
    grid.size <- res(grid)

    # Same as getAOO
    input.points <- rasterToPoints(input.data)
    xy <- as.matrix(input.points)[,c(1,2)] # select xy column only
    x <- rasterize(xy, grid, fun='count') # returns a 10 * 10 raster where cell value is the number of points in the cell
    names(x) <- 'count'
    grid.shp <- rasterToPolygons(x, dissolve=FALSE)
    if (bottom.1pct.rule == FALSE){
      outGrid <- grid.shp
    }
    if (bottom.1pct.rule == TRUE){
      cell.res <- res(input.data)
      area <- cell.res[1] * cell.res[2]
      one.pc.grid <- grid.size[1] * grid.size[2] / 100 # 1pc of grid cell
      threshold <- one.pc.grid * percent / area
      outGrid <- grid.shp[grid.shp$count > threshold, ] # select only grids that meet one percent threshold
    }

    # end getAOO
    AOO.number <- length(outGrid)
    return(list(AOO.number = AOO.number,
                out.grid = outGrid))
  }

#' @export
getAOOSilent.SpatialPoints <-
  function(input.data, grid, bottom.1pct.rule = TRUE, percent = 1){

    if (bottom.1pct.rule == T) {
      stop("bottom.1pct.rule cannot be used when input is SpatialPoints as
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
  function(input.data, grid, bottom.1pct.rule = TRUE, percent = 1){
    # Different from getAOO
    grid <- grid
    grid.size <- res(grid)

    #Same as getAOO
    x <- rasterize(input.data, grid, getCover = T)
    names(x) <- 'count'
    grid.shp <- rasterToPolygons(x, dissolve = F)
    if (bottom.1pct.rule == FALSE){
      outGrid <- grid.shp[grid.shp$count > 0, ]
    }
    if (bottom.1pct.rule == TRUE){
      outGrid <- grid.shp[grid.shp$count > (percent / 100), ]
    }

    # end getAOO
    AOO.number <- length(outGrid)
    return(list(AOO.number = AOO.number,
                out.grid = outGrid))
  }
