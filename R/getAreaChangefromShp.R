#' Area Change from  Polygons.
#'
#' \code{getAreaChangefromShp} reports the difference in area of two polygon
#' datasets
#' @param x Polygon of distribution
#' @param y Polygon of distribution
#' @return Returns the difference in area of two polygon objects in km2
#' @author Nicholas Murray \email{murr.nick@@gmail.com}
#' @seealso \code{\link{getArea}} \code{\link{getAreaChangefromRast}}
#' \code{\link{getAreaChange}}
#' @examples
#' getAreaChangefromShp(p1, p2) # distribution rasters

getAreaChangefromShp <- function(p1, p2){
  a1 <- gArea(p1)
  a2 <- gArea(p2)
  a.dif.m2 <- a1 - a2
  a.dif.km2 <- a.dif.m2/1000000
  return (a.dif.km2)
}
