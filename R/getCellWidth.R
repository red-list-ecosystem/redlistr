#' Cell Width of Raster
#'
#' \code{getCellWidth} reports the width (in metres) of each raster cell.
#' datasets
#' @param x Raster object
#' @return Returns the width of each cell in a raster object
#' @author Nicholas Murray \email{murr.nick@@gmail.com}
#' @seealso \code{\link{getArea}}
#' @examples
#' getCellWidth(x) # distribution raster

getCellWidth <- function (x){
  cell.res <- res(x)
  cellwidth <- cell.res[1]
  return(cellwidth)
}
