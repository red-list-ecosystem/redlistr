#' Create empty Area of Occupancy (AOO) Grid.
#'
#' `createGrid` produces empty grid which can be used as the basis to help
#' compute AOO.
#'
#' @param input_data Spatial object (sf or SpatRaster) of an ecosystem or species distribution.
#'   Please use a CRS with units measured in metres.
#' @param cell_size A number specifying the width of the desired grid square (in
#'   same units as your coordinate reference system)
#' @return A regular grid raster with extent `input_data` expanded by two
#' cells in each direction and grid size `cell_size`. Each grid square has a
#' unique raster value that serves as its identification number.
#' @author Nicholas Murray \email{murr.nick@@gmail.com}, Calvin Lee
#'   \email{calvinkflee@@gmail.com}, Aniko B. Toth \email{anikobtoth@@gmail.com}
#' @family AOO functions
#' @references Bland, L.M., Keith, D.A., Miller, R.M., Murray, N.J. and
#'   Rodriguez, J.P. (eds.) 2016. Guidelines for the application of IUCN Red
#'   List of Ecosystems Categories and Criteria, Version 1.0. Gland,
#'   Switzerland: IUCN. ix + 94pp. Available at the following web site:
#'   <https://iucnrle.org/>

createGrid <- function(input_data, cell_size = 10000){
  grid <- terra::rast(ext(input_data), res = cell_size, crs = crs(input_data))
  grid.expanded <- terra::extend(grid, c(2,2)) # grow the grid by 2 each way
  grid.expanded[] <- 1:(ncell(grid.expanded))  # number the cells
  return (grid.expanded)
}


#' Identify positions of the bottom 1% of ecosystem area in AOO grid
#'
#' `top_pct` returns the vector positions of the largest elements
#' collectively comprising a given percentage more of the vector sum. This function
#' helps perform the bottom_1pct_rule when selecting the AOO grid by
#' identifying grid positions to keep.
#'
#' @param v A numeric vector.
#' @param pct percent of area to drop
#' @return a numeric vector indicating the indeces of the elements to keep.
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
#' @param names_from the name of the column containing ecosystem labels
#' @param bottom_1pct_rule Logical. If `TRUE`, grid cells containing the least
#' ecosystem area are dropped up to 1% of the total distribution.
#' @param percent Numeric. The minimum percent to be applied as a threshold for
#'   the `bottom_1pct_rule`
#' @param jitter logical. Whether grid randomization should be applied to units with low grid counts.
#' @param n_jitter the number of grids to test for ecosystems near
#' the AOO thresholds. Ignored if jitter = FALSE.
#' @return A shapefile of grid cells occupied by an ecosystem or species, or a
#' list of these if multiple ecosystems were input.
#' @author Nicholas Murray \email{murr.nick@@gmail.com}, Calvin Lee
#'   \email{calvinkflee@@gmail.com}, Aniko B. Toth \email{anikobtoth@@gmail.com}
#' @family AOO functions
#' @references Bland, L.M., Keith, D.A., Miller, R.M., Murray, N.J. and
#'   Rodriguez, J.P. (eds.) 2016. Guidelines for the application of IUCN Red
#'   List of Ecosystems Categories and Criteria, Version 1.0. Gland,
#'   Switzerland: IUCN. ix + 94pp. Available at the following web site:
#'   <https://iucnrle.org/>
#' @examples
#' m <- matrix(sample(1:4, 500, replace = TRUE, prob = c(4,1,1,6)), nrow=25, ncol=20)
#' r1 <- terra::rast(m, crs = "EPSG:32755")
#' AOO_grid <- makeAOOGrid(r1, cell_size = 3)
#' @export


makeAOOGrid <- function(input_data, cell_size = 10000, names_from = NA, bottom_1pct_rule = TRUE, percent = 1, jitter = TRUE, n_jitter = 35) {
  UseMethod("makeAOOGrid", input_data)
}

#' @export
makeAOOGrid.SpatRaster <-
  function(input_data, cell_size, names_from = NA, bottom_1pct_rule = TRUE, percent = 1, jitter = TRUE, n_jitter = 35) {

    if (is.lonlat(input_data)) { # check CRS
      stop("AOO cannot be calculated in a geographic coordinate reference system. Use terra::project() to change to a planar CRS.")
    }  # check CRS


    grid <- createGrid(input_data, cell_size)

    if(jitter){
      dx <- stats::runif(1, -1, 1) * cell_size/2
      dy <- stats::runif(1, -1, 1) * cell_size/2

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

    input.points <- terra::as.points(input_data)
    x <- split(input.points, names(input.points)) |>
      lapply(rasterizeGeom, grid, fun = "count") |> # returns 10 * 10 rasters where cell value is the number of points in the cell
      lapply(function(.x){
        names(.x) <- "count"
        .x
      })
    AOO_grid <- lapply(x, as.polygons, dissolve=FALSE) |> lapply(function(.x) .x[.x$count > 0,]) # remove grid cells with no instances of the ecosystem

    if(bottom_1pct_rule){
      AOO_grid <- lapply(AOO_grid, function(.x) .x[top_pct(.x$count, pct = 100-percent)])
    }

    AOO_grid <- lapply(AOO_grid, st_as_sf)

   return(AOO_grid)
  }

#' @export
makeAOOGrid.sf <-
  function(input_data, cell_size = 10000, names_from = NA, bottom_1pct_rule = TRUE, percent = 1, jitter = TRUE, n_jitter = 35) {
    # deal with any invalid geometries early.
    if(any(!st_is_valid(input_data))){
      input_data <- st_make_valid(input_data)
    }
    # check CRS
    if (st_is_longlat(input_data)) { # check CRS
      stop("AOO cannot be calculated in a geographic coordinate reference system. Use st_transform() to change to a planar CRS.")
    }

    names_from <- dplyr::coalesce(names_from, "ecosystem_name")

    # identify name field
    if (!any(colnames(input_data) %in% names_from)) {                 # check for ecosystem names
      input_data <- input_data |> dplyr::mutate(ecosystem_name = "unnamed ecosystem type")  #put new name label if not present
    }
    ecosystem_names <- dplyr::pull(input_data, !!names_from)  # pull ecosystem names

    # create assessment grid
    grid <- createGrid(input_data, cell_size)
    if(jitter){
      dx <- stats::runif(1, -1, 1) * cell_size/2
      dy <- stats::runif(1, -1, 1) * cell_size/2

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
    if (all(st_geometry_type(input_data) %in% c("POINT", "LINESTRING", "MULTIPOINT", "MULTILINESTRING", "POLYGON", "MULTIPOLYGON"))){

      if(all(st_geometry_type(input_data) %in% c("POINT", "MULTIPOINT"))){                ## Case where all inputs are points

          x <- input_data |> split(st_drop_geometry(input_data[names_from])) |>
            lapply(st_coordinates) |>
            lapply(rasterize, grid, fun='count') # returns raster where value is the number of points in cell
          AOO_grid <- lapply(x, as.polygons, dissolve=FALSE)
          if(bottom_1pct_rule == T) {
           warning("bottom_1pct_rule will not be accurate when input has POINTS geometry because
                 points do not have an inherent area. Consider converting into another
                 format. Rule has been applied based on point counts in each grid square.")
          AOO_grid <- lapply(AOO_grid, function(.x) .x[top_pct(.x$count, pct = 100-percent)])
          }
          return (AOO_grid |> lapply(st_as_sf))

        } else if (all(st_geometry_type(input_data) %in% c("LINESTRING", "MULTILINESTRING"))) {  ## case where all inputs are lines
          if (st_is_longlat(input_data)) { # check CRS
            warning("AOO is being calculated in a geographic coordinate reference system. Use st_project() to change to a planar CRS first!")
          }  # check CRS
          grid_poly <- as.polygons(grid, dissolve = FALSE) |> sf::st_as_sf()

          overlaps <- st_intersection(grid_poly, input_data)
          overlaps$length_m <- st_length(overlaps) |> as.numeric()

          AOO <- overlaps |> base::split(st_drop_geometry(overlaps[names_from]))

          if(bottom_1pct_rule){
            warning("bottom_1pct_rule is not accurate when input has LINESTRING geometry because
                 lines do not have an inherent area. Consider converting into another
                 format. Rule has been applied based on LINESTRING lengths in each grid square.")
            AOO <- lapply(AOO, function(.x) .x[top_pct(.x$length_m, pct = 100-percent),])  # remove bottom 1% of ecosystem area.
           }
          AOO_grid <- AOO |> lapply(function(.x) return(grid_poly |> dplyr::filter(.data$lyr.1 %in% .x$lyr.1)))
          return(AOO_grid)


        } else if (all(st_geometry_type(input_data) %in% c("POLYGON", "MULTIPOLYGON"))) { ## case where all input geometries are POLYGON or MULTIPOLYGON

          grid_poly <- as.polygons(grid, dissolve = FALSE) |> sf::st_as_sf()

          overlaps <- withCallingHandlers(  # gets intersections and silences attribute warnings.
            st_intersection(grid_poly, input_data[,names_from]),
            warning = function(w) {
              if (grepl("attribute variables are assumed to be spatially constant", conditionMessage(w)))
                invokeRestart("muffleWarning")
            }
          )
          overlaps$area_m2 <- st_area(overlaps) |> as.numeric()

          AOO <- overlaps |> base::split(st_drop_geometry(overlaps[names_from]))

          if(bottom_1pct_rule)
            AOO <- lapply(AOO, function(.x) .x[top_pct(.x$area_m2, pct = 100-percent),])  # remove bottom 1% of ecosystem area.

          AOO_grid <- AOO |> lapply(function(.x) return(grid_poly |> dplyr::filter(.data$lyr.1 %in% .x$lyr.1)))
          return(AOO_grid)
      } else {
         stop("Multiple geometry types detected. Please enter input data with a consistent geometry")
      } # case where the input data has a mix of accepted geometries.

     } else {
      stop("Your input data contains geometries that are not supported. Please enter a dataset that has POLYGON, LINESTRING, or POINT geometry.")
     }  ## case where there are unusual geometries in the input data.
  }

#' @export
makeAOOGrid.AOOgrid <-
  function(input_data, cell_size = 10000, names_from = NA, bottom_1pct_rule = TRUE, percent = 1, jitter = TRUE, n_jitter = 35){

    # flag NA values for computation purposes
   if (inherits(input_data@input, "SpatRaster")){
      ecosystemunit <- terra::deepcopy(input_data@input)
      terra::NAflag(ecosystemunit) <- 0
   }else if(inherits(input_data@input, "sf")){
     ecosystemunit <- input_data@input
   }
   cell_size <- input_data@params$cellsize
   output <- lapply(1:n_jitter, function(x) makeAOOGrid(ecosystemunit, cell_size = cell_size, bottom_1pct_rule = bottom_1pct_rule, percent = percent, jitter = jitter)) |>
     lapply(`[[`, 1) #flatten list
   AOO_vals <- sapply(output, nrow)
   best_grid <- output[[which.min(AOO_vals)[1]]]

   output <-
     new("AOOgrid",
       name = input_data@name,
       grid = best_grid,
       AOO = nrow(best_grid),
       params = list(cellsize = cell_size, jitter = jitter, pct = percent, n = n_jitter),
       pctrule = bottom_1pct_rule,
       input = input_data@input,
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
#' @return an object of class AOOgrid or a list of AOOgrid objects. Ecosystems that received
#' an AOO of 60 cells or fewer on a first pass are run with a jittered grid with n specified by n_jitter
#' @author Nicholas Murray \email{murr.nick@@gmail.com}, Calvin Lee
#'   \email{calvinkflee@@gmail.com}, Aniko B. Toth \email{anikobtoth@@gmail.com}
#' @family AOO functions
#' @references Bland, L.M., Keith, D.A., Miller, R.M., Murray, N.J. and
#'   Rodriguez, J.P. (eds.) 2016. Guidelines for the application of IUCN Red
#'   List of Ecosystems Categories and Criteria, Version 1.0. Gland,
#'   Switzerland: IUCN. ix + 94pp. Available at the following web site:
#'   <https://iucnrle.org/>
#' @export
getAOO <-  function(input_data, cell_size = 10000, names_from = NA, bottom_1pct_rule = TRUE, percent = 1, jitter = TRUE, n_jitter = 35) {
  UseMethod("getAOO", input_data)
}


#'
#' @export
getAOO.SpatRaster <- function(input_data, cell_size = 10000, names_from = NA, bottom_1pct_rule = TRUE, percent = 1, jitter = TRUE, n_jitter = 35) {

  message("Initialising grids")
  AOO_grid <- makeAOOGrid(input_data, cell_size, bottom_1pct_rule, percent, jitter)

  message("Assembling initial grids")
  # Split raster into list of binary rasters
  values <- sort(unique(terra::values(input_data)))
  binary_rasters <- lapply(values, function(v) {
    as.numeric(input_data == v)
  }) |> setNames(paste0(names(input_data), "_value_", values))


  AOOgrid_list <- lapply(1:length(AOO_grid),
                         function(x) methods::new("AOOgrid",
                                         name = names(binary_rasters)[[x]],
                                         grid = AOO_grid[[x]],
                                         AOO = nrow(AOO_grid[[x]]),
                                         params = list(cellsize = cell_size, jitter = jitter, pct = percent),
                                         pctrule = bottom_1pct_rule,
                                         input = binary_rasters[[x]])) |>
    stats::setNames(paste0("value_", sort(unique(terra::values(input_data)))))

  # run grid jitter on units with AOO near a threshold
  if(jitter){
    message("Running jitter on units with 100 or fewer cells")
    AOOgrid_list <- lapply(seq_along(AOOgrid_list),
                           function(x) {
                             if(AOOgrid_list[[x]]@AOO <= 100 & AOOgrid_list[[x]]@AOO > 2) {
                               message(names(AOOgrid_list)[[x]])
                               message(paste("jittering n = ", n_jitter))
                               return(makeAOOGrid(AOOgrid_list[[x]], n_jitter = n_jitter, cell_size = cell_size, bottom_1pct_rule = bottom_1pct_rule, percent = percent, jitter = jitter))
                               } else { return(AOOgrid_list[[x]])}})
    AOOgrid_list <- stats::setNames(AOOgrid_list, names(AOO_grid))

    if(length(AOOgrid_list) == 1) return(AOOgrid_list[[1]]) else return(AOOgrid_list)
  }
}

#' @export
getAOO.sf <-  function(input_data, cell_size = 10000, names_from = NA, bottom_1pct_rule = TRUE, percent = 1, jitter = TRUE, n_jitter = 35){
  # deal with any invalid geometries early.
  if(any(!st_is_valid(input_data))){
    input_data <- st_make_valid(input_data)
  }

  message("Initialising grids")
  AOO_grid <- makeAOOGrid(input_data, cell_size, names_from, bottom_1pct_rule, percent, jitter)

  message("Assembling initial grids")
  names_from <- dplyr::coalesce(names_from, "ecosystem_name")
  if (!any(colnames(input_data) %in% names_from)) {                 # check for ecosystem names
    input_data <- input_data |> dplyr::mutate(ecosystem_name = "unnamed ecosystem type")  #put new name label if not present
  }
  input_split <- input_data |> split(st_drop_geometry(input_data[names_from]))

  AOOgrid_list <- lapply(1:length(AOO_grid),
                         function(x) methods::new("AOOgrid",
                                         name = names(input_split)[[x]],
                                         grid = AOO_grid[[x]],
                                         AOO = nrow(AOO_grid[[x]]),
                                         params = list(cellsize = cell_size, jitter = jitter, pct = percent),
                                         pctrule = bottom_1pct_rule,
                                         input = input_split[[x]]))


  # run grid jitter on units with AOO near a threshold
  if(jitter){
    message(paste("Running jitter on units with 100 or fewer cells, n =", n_jitter))
    AOOgrid_list <- lapply(1:length(AOOgrid_list),
                           function(x){
                             if(AOOgrid_list[[x]]@AOO <= 100 & AOOgrid_list[[x]]@AOO > 2) {
                               message(paste("jittering ", AOOgrid_list[[x]]@name))
                               return(makeAOOGrid(AOOgrid_list[[x]], names_from = names_from, cell_size = cell_size, bottom_1pct_rule = bottom_1pct_rule, jitter = jitter, percent = percent, n_jitter = n_jitter))
                               } else {return(AOOgrid_list[[x]])}
                           }  )
  }

  names(AOOgrid_list) <- names(input_split)
  if(length(AOOgrid_list) == 1) { return(AOOgrid_list[[1]])
  } else {
      return(AOOgrid_list) }

}


#' Make elbow plot to check jitter iterations
#'
#' `jplot` creates an elbow plot of the min AOO against
#' the number of grid replicates run. Jplots that fall to the
#' minimum value well before the highest n are robust.
#'
#' @param x an AOOgrid object
#' @return NULL; plots min AOO against number of reps
#' @author Aniko B. Toth \email{anikobtoth@@gmail.com}
#' @family AOO functions

#' @export

jplot <- function(x){
 z <- x@AOOvals
 if(length(z) > 1){
    data.frame(n = seq_along(z),
            min = seq_along(z) |> sapply(function(x) min(z[1:x]))) |>
   plot(type = "b", pch = 16,
        main = paste(x@name, "\n Running minimum AOO estimate from grid jitter"),
        xlab = "Number of jitter iterations",
        ylab = "Minimum number of grid cells occupied")
 } else {
   warning("Grid was not jittered")
 }

}
