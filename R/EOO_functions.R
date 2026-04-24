#' Creates Extent of occurrence (EOO) Polygon
#'
#' `makeEOO` is a generic function that creates a  minimum convex polygon
#' enclosing all occurrences of the ecosystems provided in the input data. If the input provided
#' is a raster layer, the points are taken from a buffer that has the radius of
#' half of the shorter edge of the pixel around the centroid.
#' @param input_data Spatial object of an ecosystem or species distribution.
#'   Please use a CRS with units measured in metres.
#' @param names_from name of the column containing ecosystem names.
#' If missing all features will be analysed together. Only needed for vector data.
#' @return An object of class sf representing the EOO of
#'   `input_data`, or a list of sf objects if multiple ecosystems were input.
#'   Also inherits its CRS from input_data.
#' @author Nicholas Murray \email{murr.nick@@gmail.com}, Calvin Lee
#'   \email{calvinkflee@@gmail.com}
#' @family EOO functions
#' @references IUCN 2024. Guidelines for the application of IUCN Red List of
#'   Ecosystems Categories and Criteria, Version 2.0. Keith, D.A., Ferrer-Paris,
#'   J.R., Ghoraba, S.M.M., Henriksen, S., Monyeki, M., Murray, N.J., Nicholson,
#'   E., Rowland, J., Skowno, A., Slingsby, J.A., Storeng, A.B., Valderrábano, M.
#'   & Zager, I. (Eds.) Gland, Switzerland: IUCN. ix + 94pp.
#'   <https://doi.org/10.2305/CJDF9122>
#' @examples
#' m <- matrix(sample(1:4, 500, replace = TRUE, prob = c(4,1,1,6)), nrow=25, ncol=20)
#' r1 <- terra::rast(m, crs = "EPSG:32755")
#' EOO.polygon <- makeEOO(r1)
#' @export

makeEOO <- function(input_data, names_from) UseMethod("makeEOO", input_data)

#' @export
makeEOO.SpatRaster <- function(input_data, names_from = NA){
  EOO.points <- as.points(input_data)
  input.split <- split(EOO.points, names(EOO.points))
  EOO.buffer <- lapply(input.split, buffer, width = min(res(input_data)) / 2)
  EOO.polygon <- lapply(EOO.buffer, hull)

  return(EOO.polygon)
}

#' @export
makeEOO.sf <- function(input_data, names_from = NA){
  # deal with any invalid geometries early.
  if(any(!st_is_valid(input_data))){
    input_data <- st_make_valid(input_data)
  }

  names_from <- dplyr::coalesce(names_from, "ecosystem_name")
  if (!any(colnames(input_data) %in% names_from)) {
    input_data <- input_data |> dplyr::mutate(ecosystem_name = "unnamed ecosystem type")
  }
 input.split <- split(input_data, st_drop_geometry(input_data[names_from]))
 EOO.polygon <- input.split |> lapply(st_union) |> lapply(st_convex_hull) |> lapply(st_sf)
return(EOO.polygon)
}

#' @export
makeEOO.SpatVector <- function(input_data, names_from = NA){
input_data <- st_sf(input_data)
return(makeEOO.sf(input_data, names_from))
}

#' Calculates area of the created EOO polygon and returns a summary object with useful info.
#'
#' `getEOO` calculates the area of the EOO polygon generated from
#' `makeEOO` the provided data and returns an EOO class object or a list of these with
#' defined summary and plot functions available.
#' @param input_data Spatial object of an ecosystem or species distribution.
#'   Please use a CRS with units measured in metres.
#' @param names_from name of the column containing ecosystem names.
#' If missing all features will be analysed together. Only needed for vector data.
#' @return An object of type EOO or a list of EOO objects that store the
#' EOO polygon, its area, and its input_data
#' @author Nicholas Murray \email{murr.nick@@gmail.com}, Calvin Lee
#'   \email{calvinkflee@@gmail.com}
#' @family EOO functions
#' @examples
#' m <- matrix(sample(1:4, 500, replace = TRUE, prob = c(4,1,1,6)), nrow=25, ncol=20)
#' r1 <- terra::rast(m, crs = "EPSG:32755")
#' EOO <- getEOO(r1)
#' @export
#' @import terra
#' @import sf
#' @import units

getEOO <- function(input_data, names_from = NA) UseMethod("getEOO", input_data)

#' @export
getEOO.SpatRaster<- function(input_data, names_from = NA){

values <- sort(unique(terra::values(input_data)))
  binary_rasters <- lapply(values, function(v) {
    as.numeric(input_data == v)
  }) |> setNames(paste0(names(input_data), "_value_", values))

  EOO.polygon <- makeEOO(input_data) |> lapply(st_as_sf)
  EOO.area <- lapply(EOO.polygon, st_area) |> lapply(units::set_units, "km^2")

  EOO_list <- lapply(1:length(binary_rasters),
                         function(x) new("EOO",
                                         name = names(binary_rasters)[[x]],
                                         pol = EOO.polygon[[x]],
                                         EOO = as.numeric(EOO.area[[x]]),
                                         unit = as.character(units(EOO.area[[x]])),
                                         input = binary_rasters[[x]]))

  if(length(EOO_list) == 1) return(EOO_list[[1]]) else return(EOO_list)
}

#' @export
getEOO.sf <- function(input_data, names_from = NA){

  # deal with any invalid geometries early.
  if(any(!st_is_valid(input_data))){
    input_data <- st_make_valid(input_data)
  }

  names_from <- dplyr::coalesce(names_from, "ecosystem_name")
  if (!any(colnames(input_data) %in% names_from)) {                 # check for ecosystem names
    input_data <- input_data |> dplyr::mutate(ecosystem_name = "unnamed ecosystem type")  #put new name label if not present
  }
  input_split <- input_data |> split(st_drop_geometry(input_data[names_from]))

  EOO.polygon <- makeEOO(input_data, names_from)
  EOO.area <- lapply(EOO.polygon, st_area) |> lapply(units::set_units, "km^2")

  EOO_list <- lapply(1:length(input_split),
                     function(x) new("EOO",
                                     name = names(input_split)[[x]],
                                     pol = EOO.polygon[[x]],
                                     EOO = as.numeric(EOO.area[[x]]),
                                     unit = as.character(units(EOO.area[[x]])),
                                     input = input_split[[x]]))
  names(EOO_list) <- names(input_split)

  if(length(EOO_list) == 1) return(EOO_list[[1]]) else return(EOO_list)
}

#' @export
getEOO.SpatVector <- function(input_data, names_from = NA){
  input_data <- st_sf(input_data)
  return(getEOO.sf(input_data, names_from))
}



#' Extracts the area from an EOO polygon
#'
#' `getEOOarea` wrapper that extracts the area slot of the EOO input
#' @param EOO an object of class EOO
#' @return an integer
#' @family EOO functions
#' @export

getAreaEOO <- function(EOO){
  return(EOO@EOO)
}
