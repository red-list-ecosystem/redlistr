#' Calculates the Area of a Raster.
#'
#' \code{getArea} reports the area of a raster object using the pixel counting
#' method.
#' @param x A raster object with cells containing no data identified as NA
#' @param value.to.count Optional. Value of the cells to be counted
#' @return The area of the cells of interest
#' @author Nicholas Murray \email{murr.nick@@gmail.com}, Calvin Lee
#'   \email{calvinkflee@@gmail.com}
#' @family Change functions
#' @examples
#' a.r1 <- getArea(r1) # area of all non-NA cells in r1
#' a.r1 <- getArea(r1, 1) # area of cells labelled 1 in r1
#' @export

getArea <- function(x, value.to.count){
  if(length(raster::unique(x)) != 1 & missing(value.to.count)){
    warning("The input raster is not binary, counting ALL non NA cells\n")
    cell.res <- res(x)
    cell.width <- cell.res[1]
    x.df <- plyr::count(values(x))
    n.cell <- sum(x.df[which(!is.na(x.df[, 1])), ]$freq)
    aream2 <- (cell.width * cell.width) * n.cell
    areakm2 <- aream2/1000000
    return (areakm2)
  }
  else if(length(raster::unique(x)) != 1){
    cell.res <- res(x)
    cell.width <- cell.res[1]
    x.df <- plyr::count(values(x))
    n.cell <- x.df[which(x.df[, 1] == value.to.count), ]$freq
    aream2 <- (cell.width * cell.width) * n.cell
    areakm2 <- aream2/1000000
    return (areakm2)
  }
  else{
    cell.res <- res(x)
    cell.width <- cell.res[1]
    x.df <- plyr::count(values(x))
    n.cell <- x.df[which(x.df[, 1] == TRUE), ]$freq
    aream2 <- (cell.width * cell.width) * n.cell
    areakm2 <- aream2/1000000
    return (areakm2)
  }
}

#' Area change between two inputs in km2
#'
#' \code{getAreaChang} reports the difference in area between two inputs. These
#' can be RasterLayers, SpatialPolygons, or numbers. Any combinations of these
#' inputs are valid. If using number as input, ensure it is measured in km2
#'
#' @param x RasterLayer or SpatialPolygons object of distribution or Numeric
#'   representing area in km2
#' @param y RasterLayer or SpatialPolygons object of distribution or Numeric
#'   representing area in km2
#' @return Returns the difference in area of the two inputs in km2
#' @author Nicholas Murray \email{murr.nick@@gmail.com}, Calvin Lee
#'   \email{calvinkflee@@gmail.com}
#' @family Change functions
#' @examples
#' a.dif <- getAreaChange(r1, r2) # distribution rasters
#' a.dif <- getAreaChange(r1, p2) # distribution raster and distribution polygon
#' @export

getAreaLoss <- function(x, y){
  if(class(x)[[1]] == 'RasterLayer'){
    a.x <- getArea(x)
  } else if (class(x)[[1]] == 'SpatialPolygons'){
    a.x <- gArea(x) / 1000000
  } else if (is.numeric((x))){
    a.x <- x
  } else {
    stop('x is not a RasterLayer, SpatialPolygons, or Numeric')
  }
  if(class(y)[[1]] == 'RasterLayer'){
    a.y <- getArea(y)
  } else if (class(y)[[1]] == 'SpatialPolygons'){
    a.y <- gArea(y) / 1000000
  } else if (is.numeric((y))){
    a.y <- y
  } else {
    stop('y is not a RasterLayer, SpatialPolygons, or Numeric')
  }
  a.dif.km2 <- (a.x - a.y)
  return(a.dif.km2)
}


#' Change statistics.
#'
#' \code{getDeclineStats} calculates the Proportional Rate of Decline (PRD),
#' Absolute Rate of Decline (ARD) and Absolute Rate of Change (ARC), given two
#' areas at two points in time. Also provides the total area difference.
#'
#' @param A.t1 Area at time t1
#' @param year.t1 Year of time t1
#' @param A.t2 Area at time t2
#' @param year.t2 Year of time t2
#' @param method Method(s) used to calculate rate of decline. Either 'PRD',
#'   'ARD', or 'ARC'. Defaults to include all three methods. See vignette to see
#'   a more detailed explanation for each of them.
#' @return A dataframe with total area difference, and possibly a selection of:
#' \itemize{
#'  \item Proportional Rate of Decline (PRD)
#'  \item Absolute Rate of Decline (ARD)
#'  \item Absolute Rate of Change (ARC)
#' @author Nicholas Murray \email{murr.nick@@gmail.com}, Calvin Lee
#'   \email{calvinkflee@@gmail.com}
#' @family Change functions
#' @references Bland, L.M., Keith, D.A., Miller, R.M., Murray, N.J. and
#'   Rodriguez, J.P. (eds.) 2016. Guidelines for the application of IUCN Red
#'   List of Ecosystems Categories and Criteria, Version 1.0. Gland,
#'   Switzerland: IUCN. ix + 94pp. Available at the following web site:
#'   \url{iucnrle.org/}
#'   Puyravaud, J.-P. 2003. Standardizing the calculation of the
#'   annual rate of deforestation. Forest Ecology and Management, 177, 593-596.
#' @examples
#' a.r1 <- getArea(r1) # a distribution raster
#' a.r2 <- getArea(r2) # a distribution raster
#' decline.stats <- getDeclineStats(a.r1, a.r2, year.t1 = 1990, year.t2 = 2012, method = c('ARD', 'ARC')
#' @export

getDeclineStats <- function (A.t1, A.t2, year.t1, year.t2, methods = c('ARD', 'PRD', 'ARC')){
  out <- data.frame(area.loss = (A.t1-A.t2))
  if(any(methods == 'ARD')){
    ARD <- -((A.t2-A.t1)/(year.t2-year.t1))
    # Absolute rate of change (also known as Annual Change(R)) in Puyrvaud
    out <- cbind(out, ARD = ARD)
  }
  if(any(methods == 'PRD')){
    PRD <- 100 * (1-(A.t2/A.t1)^(1/(year.t2-year.t1)))
    # Proportional rate of change (also known as trajectory (r))
    out <- cbind(out, PRD = PRD)
  }
  if(any(methods == 'ARC')){
    ARC <- (1/(year.t2-year.t1))*log(A.t2/A.t1)
    # Annual rate of change from Puyravaud 2004 (also known as instantaneous rate of change)
    out <- cbind(out, ARC = ARC)
  }
  return (out)
}

