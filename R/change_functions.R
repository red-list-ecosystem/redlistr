#' Calculates the Area of a Raster.
#'
#' \code{getArea} reports the area of a raster object using the pixel counting
#' method.
#' @param x A raster object. No data value should be NA
#' @param value.to.count Optional. Value of the cells to be counted
#' @return The area of the cells of interest
#' @author Nicholas Murray \email{murr.nick@@gmail.com}, Calvin Lee
#'   \email{calvinkflee@@gmail.com}
#' @family Change functions
#' @examples
#' crs.UTM55S <- '+proj=utm +zone=55 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs'
#' r1 <- raster(ifelse((volcano<130), NA, 1), crs = crs.UTM55S)
#' extent(r1) <- extent(0, 6100, 0, 8700)
#' a.r1 <- getArea(r1) # area of all non-NA cells in r1
#' @export
#' @import raster

getArea <- function(x, value.to.count){
  if(isLonLat(x) == T){
    stop('Input raster has a longitude/latitude CRS.\nPlease reproject to a projected coordinate system')
  }
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
#' \code{getAreaLoss} reports the difference in area between two inputs. These
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
#' crs.UTM55S <- '+proj=utm +zone=55 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs'
#' r1 <- raster(ifelse((volcano<130), NA, 1), crs = crs.UTM55S)
#' extent(r1) <- extent(0, 6100, 0, 8700)
#' r2 <- raster(ifelse((volcano<145), NA, 1), crs = crs.UTM55S)
#' extent(r2) <- extent(0, 6100, 0, 8700)
#' a.dif <- getAreaLoss(r1, r2) # distribution rasters
#' @export

getAreaLoss <- function(x, y){
  if(class(x)[[1]] == 'RasterLayer'){
    a.x <- getArea(x)
  } else if (class(x)[[1]] == 'SpatialPolygons'){
    a.x <- rgeos::gArea(x) / 1000000
  } else if (is.numeric((x))){
    a.x <- x
  } else {
    stop('x is not a RasterLayer, SpatialPolygons, or Numeric')
  }
  if(class(y)[[1]] == 'RasterLayer'){
    a.y <- getArea(y)
  } else if (class(y)[[1]] == 'SpatialPolygons'){
    a.y <- rgeos::gArea(y) / 1000000
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
#' areas at two points in time. Also provides the total area difference. Inputs
#' are usually the results from \code{getArea}.
#'
#' @param A.t1 Area at time t1
#' @param year.t1 Year of time t1
#' @param A.t2 Area at time t2
#' @param year.t2 Year of time t2
#' @param methods Method(s) used to calculate rate of decline. Either 'PRD',
#'   'ARD', and/or 'ARC'. See vignette to see a more detailed explanation for
#'   each of them.
#' @return A dataframe with total area difference, and a selection of:
#' \itemize{
#'  \item Proportional Rate of Decline (PRD)
#'  \item Absolute Rate of Decline (ARD)
#'  \item Absolute Rate of Change (ARC)
#'  }
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
#' a.r1 <- 23.55
#' a.r2 <- 15.79
#' decline.stats <- getDeclineStats(a.r1, a.r2, year.t1 = 1990, year.t2 = 2012,
#'                                  methods = c('ARD', 'ARC'))
#' @export

getDeclineStats <- function (A.t1, A.t2, year.t1, year.t2,
                             methods){
  out <- data.frame(area.loss = (A.t1-A.t2))
  if(missing(methods)){
    stop("Please select method(s) to be used for calculating the rate of decline.")
  }
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

#' Future Area Estimate
#'
#' \code{futureAreaEstimate} calculates the expected area of a distribution at a
#' future date using known rates of decline.
#'
#' @param A.t1 Area at time t1
#' @param year.t1 Year of time t1
#' @param nYears Number of years since t1 for area prediction
#' @param ARD Absolute rate of decline
#' @param PRD Proportional rate of decline
#' @param ARC Annual rate of change
#' @return A dataframe with the forecast year, and a combination of:
#' \itemize{
#'  \item Future area as estimated with absolute rate of decline (ARD)
#'  \item Future area as estimated with proportional rate of decline (PRD)
#'  \item Future area as estimated with absolute rate of change (ARC)
#'  }
#' @author Nicholas Murray \email{murr.nick@@gmail.com}, Calvin Lee
#'   \email{calvinkflee@@gmail.com}
#' @family change_functions
#' @references Bland, L.M., Keith, D.A., Miller, R.M., Murray, N.J. and
#'   Rodriguez, J.P. (eds.) 2016. Guidelines for the application of IUCN Red
#'   List of Ecosystems Categories and Criteria, Version 1.0. Gland,
#'   Switzerland: IUCN. ix + 94pp. Available at the following web site:
#'   \url{iucnrle.org/}
#' @examples
#' a.r1 <- 23.55
#' a.r2 <- 15.79
#' decline.stats <- getDeclineStats(a.r1, a.r2, year.t1 = 1990, year.t2 = 2012,
#'                        methods = 'PRD')
#' a.2040.PRD <- futureAreaEstimate(a.r1, a.r2, year.t1 = 1990, nYears = 50,
#'                                  PRD = decline.stats$PRD)
#' @export

futureAreaEstimate <- function(A.t1, year.t1, nYears, ARD = NA, PRD = NA, ARC = NA){
  y.t3 <- year.t1+nYears
  out <- data.frame(forecast.year = y.t3)
  if(!is.na(ARD)){
    A.ARD.t3 <- A.t1 - (ARD*nYears)
    if(A.ARD.t3 < 0) A.ARD.t3 = 0
    out <- cbind(out, A.ARD.t3 = A.ARD.t3)
  }
  if(!is.na(PRD)){
    A.PRD.t3 <- A.t1 * (1 -(PRD/100))^nYears
    if(A.PRD.t3 < 0) A.PRD.t3 = 0
    out <- cbind(out, A.PRD.t3 = A.PRD.t3)
  }
  if(!is.na(ARC)){
    A.ARC.t3 <- A.t1 * (1 + ARC)^nYears
    if(A.ARC.t3 < 0) A.ARC.t3 = 0
    out <- cbind(out, A.ARC.t3 = A.ARC.t3)
  }
  if(all(c(is.na(PRD), is.na(ARD), is.na(ARC)))){
    stop("Please input at least one of 'ARD', 'PRD', or 'ARC'.")
  }
  return(out)
}
