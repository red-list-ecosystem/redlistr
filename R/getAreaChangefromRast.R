#' Area Change from  Raster.
#'
#' \code{getAreaChangefromRast} reports the difference in area of two raster
#' objects.
#'
#' @param x Raster object of distribution
#' @param y Raster object of distribution
#' @return Returns the difference in area of two raster objects in km2
#' @author Nicholas Murray \email{murr.nick@@gmail.com}
#' @seealso \code{\link{getArea}} \code{\link{getAreaChangefromShp}}
#' \code{\link{getAreaChange}}
#' @examples
#' getAreaChangefromRast(r1, r2) # distribution rasters

getAreaChangefromRast <- function (x, y){
  a1 <- getArea (x)
  a2 <- getArea (y)
  a.dif <- a1 - a2
  return(a.dif)
}
