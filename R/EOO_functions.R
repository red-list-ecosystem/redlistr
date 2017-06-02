#' Creates Extent of occurrence (EOO) Polygon
#'
#' \code{makeEOO} creates a  minimum convex polygon enclosing all occurrences of
#' the provided data
#' @param ecosystem.data Raster object of an ecosystem or species distribution.
#'   Please use a CRS with units measured in metres.
#' @return An object of class SpatialPolygons representing the EOO of
#'   \code{ecosystem.data}. Also inherits its CRS.
#' @author Nicholas Murray \email{murr.nick@@gmail.com}, Calvin Lee
#'   \email{calvinkflee@@gmail.com}
#' @family EOO functions
#' @references Bland, L.M., Keith, D.A., Miller, R.M., Murray, N.J. and
#'   Rodriguez, J.P. (eds.) 2016. Guidelines for the application of IUCN Red
#'   List of Ecosystems Categories and Criteria, Version 1.0. Gland,
#'   Switzerland: IUCN. ix + 94pp. Available at the following web site:
#'   \url{iucnrle.org/}
#' @examples
#' EOO.polygon <- makeEOO(r1)

makeEOO <- function (ecosystem.data){
  # Makes an EOO spatial polygon using the centre point of each pixel as the boundary
  EOO.points <- rasterToPoints(ecosystem.data)
  if (nrow(EOO.points) <= 1) { # handling single pixels since chull fails for 1 pixel
    EOO.polygon <- rasterToPolygons(ecosystem.data)
  } else {
    EOO.chull <- chull(EOO.points)
    EOO.envelope <- EOO.points[EOO.chull,]
    c <- data.frame(EOO.envelope) # turn into a dataframe
    c[,3] <- NULL # get rid of the ID column ## FAILS HERE WHEN ECOSYSTEM IS 1 Pixel
    d <- as.matrix(c) #sp package needs a matrix
    e <- rbind(d, d[1,]) #this adds a row at the bottom to close the ring
    f <- Polygon(e) # creates a polygon object
    g <- Polygons(list(f), 1) # wrap it in a polygons object
    EOO.polygon <- SpatialPolygons(list(g)) # wrap it in a spatial polygons object
  }
  proj4string(EOO.polygon) <- crs(ecosystem.data)
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
#' EOO.area <- getAreaEOO(EOO.polygon)

getAreaEOO <- function(EOO.polygon){
  # Returns the area of the makeEOO output (spatialpolygons object)
  EOO.aream2 <- sapply(slot(EOO.polygon, "polygons"), slot, "area")
  # get the area from the slots in the polygon dataset
  EOO.areakm2 <- EOO.aream2/1000000
  return(EOO.areakm2)
}

