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

# getArea <- function (x){
#   cell.res <- res(x)
#   cell.width <- cell.res[1]
#   n.cell <- ncell( ! is.na(x[]) )# count non NA cells
#   aream2 <- (cell.width * cell.width) * n.cell
#   areakm2 <- aream2/1000000
#   return (areakm2)
# }

getArea <- function (x){
  cell.res <- res(x)
  cell.width <- cell.res[1]
  values(x)[values(x) == 0] <- NA
  n.cell <- ncell(Which(!is.na(x), cells=TRUE)) # count non NA cells
  aream2 <- (cell.width * cell.width) * n.cell
  areakm2 <- aream2/1000000
  return (areakm2)
}


freq(inRast)
freq(x)

x = inRast
x <- inRast == 1 #convert to single class
x <- inRast == 3 #convert to single class

getArea(x)

time.test = microbenchmark(
  rast <- inRast == 2, #convert to single class
  values(rast)[values(rast) == 0] <- NA,
  n.cell <- ncell(Which(!is.na(rast), cells=TRUE)), # this is faster
  n.cell.2 <- sum(na.omit(getValues(rast))>0) # this one works but is a tiny bit slower
)


library(ggplot2)
autoplot(time.test)


