#' Creates Extent of occurrence (EOO) Polygon
#'
#' \code{makeEOO} creates a  minimum convex polygon enclosing all occurrences of
#' the provided data
#' @param input.data Object of an ecosystem or species distribution. Accepts
#'   either raster, spatial points or spatial polygons. Please use a CRS with
#'   units measured in metres.
#' @return An object of class SpatialPolygons representing the EOO of
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
#' r1 <- raster(ifelse((volcano<130), NA, 1), crs = crs.UTM55S)
#' extent(r1) <- extent(0, 6100, 0, 8700)
#' EOO.polygon <- makeEOO(r1)
#' @export
#' @import sp
#' @import raster
#' @import rgeos

makeEOO <- function(input.data){
  if (class(input.data) ==  "SpatialPolygonsDataFrame")
  {
    # Uses rgeos::gConvexHull
    EOO.polygon <- rgeos::gConvexHull(input.data)
  }
  else {if(class(input.data) == "RasterLayer"){
    # Makes an EOO spatial polygon using the centre point of each pixel as the boundary
    EOO.points <- rasterToPoints(input.data)
  }

    EOO.points <- input.data@coords # accessing coordinates of shapefile
    if (nrow(EOO.points) <= 1) { # handling single pixels since chull fails for 1 pixel
      EOO.polygon <- rasterToPolygons(input.data)
    } else {
      EOO.chull <- grDevices::chull(EOO.points)
      EOO.envelope <- EOO.points[EOO.chull,]
      EOO.polygon <- SpatialPolygons(list(Polygons(list(Polygon(EOO.envelope[, 1:2])),
                                                   ID=1)))
    }
  }
  proj4string(EOO.polygon) <- crs(input.data)
  return(EOO.polygon)
}

#' Calculates area of the created EOO polygon.
#'
#' \code{getAreaEOO} calculates the area of the EOO polygon generated from
#' \code{makeEOO} the provided data
#' @param EOO.polygon An object of class SpatialPolygons, usually the output
#'   from \code{makeEOO}.
#' @return The area of the \code{EOO.polygon} in km2
#' @author Nicholas Murray \email{murr.nick@@gmail.com}, Calvin Lee
#'   \email{calvinkflee@@gmail.com}
#' @family EOO functions
#' @examples
#' crs.UTM55S <- '+proj=utm +zone=55 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs'
#' r1 <- raster(ifelse((volcano<130), NA, 1), crs = crs.UTM55S)
#' extent(r1) <- extent(0, 6100, 0, 8700)
#' EOO.polygon <- makeEOO(r1)
#' EOO.area <- getAreaEOO(EOO.polygon)
#' @export

getAreaEOO <- function(EOO.polygon){
  # Returns the area of the makeEOO output (spatialpolygons object)
  EOO.aream2 <- sapply(methods::slot(EOO.polygon, "polygons"), methods::slot, "area")
  # get the area from the slots in the polygon dataset
  EOO.areakm2 <- EOO.aream2/1000000
  return(EOO.areakm2)
}
