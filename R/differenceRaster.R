#' Image Differencing.
#'
#' @param r1 Raster object of an ecosystem or species distribution at t1
#' @param r2 Raster object of an ecosystem or species distribution at t1
#' @return Returns a raster object of the area of change between \code{r1} and
#'   \code{r2}.
#' @author Nicholas Murray \email{murr.nick@@gmail.com}
#' @examples
#' a = differenceRaster(raster1, raster2)
#' plot (raster1, col = "black")
#' plot (raster2, col = "darkgrey", add = TRUE)
#' plot(a, col = "red", add = TRUE)

differenceRaster <- function (r1, r2){
  p1 <- rasterToPolygons(r1, dissolve = T)
  p2 <- rasterToPolygons(r2, dissolve = T)
  dif.p <- gDifference(p1, p2)
  dif.r <- rasterize(dif.p, r1)
  return (dif.r)
}

# TODO: what happens if they don't overlap??
