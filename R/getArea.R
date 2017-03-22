#' Area of Raster.
#'
#' \code{getArea} reports the area of a raster object using the pixel counting
#' method. Requires a binary raster as input.
#'
#' @param x A binary raster object with cells containing no data identified as
#' NA
#' @return Returns the area of a raster object in km2
#' @author Nicholas Murray \email{murr.nick@@gmail.com}
#' @seealso \code{\link{getCellRes}}
#' @examples
#' a.r1 = getArea(r1) # a distribution raster

getArea <- function (x){
  cell.res <- res(x)
  cell.width <- cell.res[1]
  n.cell <- ncell( ! is.na(x[]) )# count non NA cells
  areakm2 <- aream2/1000000
  return (areakm2)
}

x = rast
ncell(x)
ncell(Which(!is.na(x), cells=TRUE))
?Which
ncell(!is.na(x))
?na.omit
ncell ()


