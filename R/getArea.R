#' Calculates the Area of a Raster.
#'
#' \code{getArea} reports the area of a raster object using the pixel counting
#' method.
#' @param x A raster object with cells containing no data identified as NA
#' @param value.to.count Optional. Value of the cells to be counted
#' @return The area of the cells of interest
#' @author Nicholas Murray \email{murr.nick@@gmail.com}, Calvin Lee
#'   \email{calvinkflee@@gmail.com}
#' @examples
#' a.r1 <- getArea(r1) # area of all non-NA cells in r1
#' a.r1 <- getArea(r1, 1) # area of cells labelled 1 in r1


getArea <- function(x, value.to.count){
  if(length(raster::unique(x)) != 1 & missing(value.to.count)){
    warning("The input raster is not binary, counting ALL non NA cells\n")
    cell.res <- res(x)
    cell.width <- cell.res[1]
    n.cell <- sum(plyr::count(values(x))$freq)
    aream2 <- (cell.width * cell.width) * n.cell
    areakm2 <- aream2/1000000
    return (areakm2)
  }
  else if(length(raster::unique(x)) != 1){
    cell.res <- res(x)
    cell.width <- cell.res[1]
    x.df <- plyr::count(values(x))
    n.cell <- x.df[which(x.df[,1] == value.to.count),]$freq
    aream2 <- (cell.width * cell.width) * n.cell
    areakm2 <- aream2/1000000
    return (areakm2)
  }
  else{
    cell.res <- res(x)
    cell.width <- cell.res[1]
    n.cell <- plyr::count(values(x))$freq
    aream2 <- (cell.width * cell.width) * n.cell
    areakm2 <- aream2/1000000
    return (areakm2)
  }
}



