#' Creates Extent of occurrence (EOO) Polygon
#'
#' \code{makeEOO} is a generic function that creates a  minimum convex polygon
#' enclosing all occurrences of the provided spatial data
#' @param input.data Spatial object of an ecosystem or species distribution.
#'   Please use a CRS with units measured in metres.
#' @return An object of class SpatVect representing the EOO of
#'   \code{input.data}. Also inherits its CRS.
#' @author Nicholas Murray \email{murr.nick@@gmail.com}, Calvin Lee
#'   \email{calvinkflee@@gmail.com}
#' @family EOO functions
#' @references Bland, L.M., Keith, D.A., Miller, R.M., Murray, N.J. and
#'   Rodriguez, J.P. (eds.) 2016. Guidelines for the application of IUCN Red
#'   List of Ecosystems Categories and Criteria, Version 1.0. Gland,
#'   Switzerland: IUCN. ix + 94pp. Available at the following web site:
#'   \url{https://iucnrle.org/}
#' @examples
#' crs.UTM55S <- '+proj=utm +zone=55 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs'
#' r1 <- rast(ifelse((volcano<130), NA, 1), crs = crs.UTM55S)
#' extent(r1) <- extent(0, 6100, 0, 8700)
#' EOO.polygon <- makeEOO(r1)
#' @export
#' @import sp
#' @import raster
#' @import terra

makeEOO <- function(input.data) UseMethod("makeEOO", input.data)

#' @export
makeEOO.RasterLayer <- function(input.data){
  input_rast <- rast(input.data)

  EOO.points <- as.points(input_rast)
  EOO.polygon <- convHull(EOO.points)
  return(EOO.polygon)
}

#' @export
makeEOO.SpatRaster <- function(input.data){
  EOO.points <- as.points(input.data)
  EOO.polygon <- convHull(EOO.points)
  return(EOO.polygon)
}

#' @export
makeEOO.SpatialPoints <- function(input.data){
  input_vect <- vect(input.data)
  EOO.polygon <- convHull(input_vect)
  return(EOO.polygon)
}

#' @export
makeEOO.SpatialPolygons <- function(input.data){
  input_vect <- vect(input.data)
  EOO.polygon <- convHull(input_vect)
  return(EOO.polygon)
}

makeEOO.SpatVector <- function(input.data){
  EOO.polygon <- convHull(input.data)
  return(EOO.polygon)
}

#' Calculates area of the created EOO polygon.
#'
#' \code{getAreaEOO} calculates the area of the EOO polygon generated from
#' \code{makeEOO} the provided data
#' @param EOO.polygon An object of class SpatVect, usually the output
#'   from \code{makeEOO}.
#' @param unit Character. Output unit of area. One of "m", "km", or "ha"
#' @return The area of the \code{EOO.polygon} in km2
#' @author Nicholas Murray \email{murr.nick@@gmail.com}, Calvin Lee
#'   \email{calvinkflee@@gmail.com}
#' @family EOO functions
#' @examples
#' crs.UTM55S <- '+proj=utm +zone=55 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs'
#' r1 <- rast(ifelse((volcano<130), NA, 1), crs = crs.UTM55S)
#' extent(r1) <- extent(0, 6100, 0, 8700)
#' EOO.polygon <- makeEOO(r1)
#' EOO.area <- getAreaEOO(EOO.polygon)
#' @export

getAreaEOO <- function(EOO.polygon, unit = "km"){
  EOO.area <- expanse(EOO.polygon, unit)
  return(EOO.area)
}
