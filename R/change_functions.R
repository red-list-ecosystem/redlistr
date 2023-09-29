#' Calculates the Area of a Raster.
#'
#' `getArea` reports the area of a RasterLayer object using the pixel
#'  counting method, or terra::expanse for SpatRaster and SpatVector objects,
#'  or the area of a SpatialPolygons or sf object using sf::st_area
#' @param x Either a RasterLayer or SpatialPolygons object. For a RasterLayer,
#'   no data value should be NA
#' @param ... Addition arguments based on input format
#' @return The total area of the cells of interest in km2
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
#' @import terra
#' @import sf

getArea <- function(x, ...){
  if(isLonLat(x)){
    stop('Input raster has a longitude/latitude CRS.\nPlease reproject to a projected coordinate system')
  }
  UseMethod("getArea", x)
}

#' Calculates the Area of a Raster from RasterLayer.
#'
#' `getArea` reports the area of a RasterLayer object using the pixel
#'  counting method.
#' @param x Either a RasterLayer object. No data value should be NA
#' @param value.to.count Optional. Value of the cells in a RasterLayer to be
#'   counted
#' @param ... Addition arguments based on input format
#' @return The total area of the cells of interest in km2
#' @author Nicholas Murray \email{murr.nick@@gmail.com}, Calvin Lee
#'   \email{calvinkflee@@gmail.com}
#' @family Change functions
#' @export
getArea.RasterLayer <- function(x, value.to.count, ...){
  if(length(raster::unique(x)) != 1 & missing(value.to.count)){
    warning("The input raster is not binary, counting ALL non NA cells\n")
    cell.res <- res(x)
    cell.area <- cell.res[1] * cell.res[2]
    x.df <- plyr::count(values(x))
    n.cell <- sum(x.df[which(!is.na(x.df[, 1])), ]$freq)
    aream2 <- cell.area * n.cell
    areakm2 <- aream2/1000000
    return (areakm2)
  }
  else if(length(raster::unique(x)) != 1){
    cell.res <- res(x)
    cell.area <- cell.res[1] * cell.res[2]
    x.df <- plyr::count(values(x))
    n.cell <- x.df[which(x.df[, 1] == value.to.count), ]$freq
    aream2 <- cell.area * n.cell
    areakm2 <- aream2/1000000
    return (areakm2)
  }
  else{
    cell.res <- res(x)
    cell.area <- cell.res[1] * cell.res[2]
    x.df <- plyr::count(values(x))
    n.cell <- x.df[which(!is.na(x.df[,1])), ]$freq
    aream2 <- cell.area * n.cell
    areakm2 <- aream2/1000000
    return (areakm2)
  }
}

#' Calculates the Area of a Raster from SpatVect.
#'
#' `getArea` reports the area of a SpatVect. object using terra::expanse
#' @param x A SpatVect object
#' @param ... Addition arguments based on input format
#' @return The total area of the cells of interest in km2
#' @author Nicholas Murray \email{murr.nick@@gmail.com}, Calvin Lee
#'   \email{calvinkflee@@gmail.com}
#' @family Change functions
#' @export
getArea.SpatVect <- function(x, ...){
  area <- expanse(x, "km")
  return(area)
}

#' Calculates the Area of a Raster from SpatRaster.
#'
#' `getArea` reports the area of a SpatRaster object using terra::expanse
#' @param x SpatRaster
#' @param byValue Logical. If TRUE, the area for each unique cell value is
#'    returned.
#' @param ... Addition arguments based on input format
#' @return The total area of the cells of interest in km2
#' @author Nicholas Murray \email{murr.nick@@gmail.com}, Calvin Lee
#'   \email{calvinkflee@@gmail.com}
#' @family Change functions
#' @export
getArea.SpatRaster <- function(x, byValue, ...){
  area <- expanse(x, "km", byValue)
  return(area)
}

#' Calculates the Area of a Raster from SpatialPolygons.
#'
#' `getArea` reports the area of a SpatialPolygons object using sf::st_area
#' @param x A SpatialPolygons object.
#' @param ... Addition arguments based on input format
#' @return The total area of the cells of interest in km2
#' @author Nicholas Murray \email{murr.nick@@gmail.com}, Calvin Lee
#'   \email{calvinkflee@@gmail.com}
#' @family Change functions
#' @export
getArea.SpatialPolygons <- function(x, ...){
  sf_polygon <- st_as_sf(x)
  area <- st_area(sf_polygon)
  return(area)
}

#' Calculates the Area of a Raster from sf object
#'
#' `getArea` reports the area of a sf object using sf::st_area
#' @param x A sf object
#' @param ... Addition arguments based on input format
#' @return The total area of the cells of interest in km2
#' @author Nicholas Murray \email{murr.nick@@gmail.com}, Calvin Lee
#'   \email{calvinkflee@@gmail.com}
#' @family Change functions
#' @export
getArea.sf <- function(x, ...){
  area <- st_area(x) / 1000000
  return(as.numeric(area))
}

#' Area change between two inputs in km2
#'
#' `getAreaLoss` reports the difference in area between two inputs. These
#' can be RasterLayers, SpatialPolygons, SpatRaster, SpatVect, sf or numbers.
#' Any combinations of these inputs are valid. If using number as input, ensure
#' it is measured in km2
#'
#' @param x Spatial obect or numeric representing area in km2
#' @param y Spatial object or numeric representing area in km2
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
  if(inherits(x, 'RasterLayer') | inherits(x, 'SpatialPolygons') |
     inherits(x, 'SpatRaster') | inherits(x, 'SpatVect') | inherits(x, 'sf')){
    a.x <- getArea(x)
  } else if (is.numeric((x))){
    a.x <- x
  } else {
    stop('x is not a RasterLayer, SpatialPolygons, SpatRaster, SpatVect, sf,
         or Numeric')
  }
  if(inherits(y, 'RasterLayer') | inherits(y, 'SpatialPolygons') |
     inherits(y, 'SpatRaster') | inherits(y, 'SpatVect') | inherits(y, 'sf')){
    a.y <- getArea(y)
  } else if (is.numeric((y))){
    a.y <- y
  } else {
    stop('y is not a RasterLayer, SpatialPolygons, SpatRaster, SpatVect, sf,
         or Numeric')
  }
  a.dif.km2 <- (a.x - a.y)
  return(a.dif.km2)
}


#' Change statistics.
#'
#' `getDeclineStats` calculates the Proportional Rate of Decline (PRD),
#' Absolute Rate of Decline (ARD) and Annual Rate of Change (ARC), given two
#' areas at two points in time. Also provides the total area difference. Inputs
#' are usually the results from `getArea`.
#'
#' @param A.t1 Area at time t1
#' @param year.t1 Year of time t1
#' @param A.t2 Area at time t2
#' @param year.t2 Year of time t2
#' @param methods Method(s) used to calculate rate of decline. Either 'PRD',
#'   'ARD', and/or 'ARC'. See vignette to see a more detailed explanation for
#'   each of them.
#' @return A dataframe with absolute differences between the two inputs, and a
#'   selection of:
#' \itemize{
#'  \item Proportional Rate of Decline (PRD)
#'  \item Absolute Rate of Decline (ARD)
#'  \item Annual Rate of Change (ARC)
#'  }
#' @author Nicholas Murray \email{murr.nick@@gmail.com}, Calvin Lee
#'   \email{calvinkflee@@gmail.com}
#' @family Change functions
#' @references Bland, L.M., Keith, D.A., Miller, R.M., Murray, N.J. and
#'   Rodriguez, J.P. (eds.) 2016. Guidelines for the application of IUCN Red
#'   List of Ecosystems Categories and Criteria, Version 1.0. Gland,
#'   Switzerland: IUCN. ix + 94pp. Available at the following web site:
#'   <https://iucnrle.org/>
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
  out <- data.frame(absolute.loss = (A.t1-A.t2))
  if(missing(methods)){
    stop("Please select method(s) to be used for calculating the rate of decline.")
  }
  if(any(methods == 'ARD')){
    ARD <- -((A.t2-A.t1)/(year.t2-year.t1))
    # Absolute rate of decline (also known as Annual Change(q)) in Puyrvaud
    out <- cbind(out, ARD = ARD)
  }
  if(any(methods == 'PRD')){
    PRD <- 100 * (1-(A.t2/A.t1)^(1/(year.t2-year.t1)))
    # Proportional rate of change (also known as trajectory (r))
    out <- cbind(out, PRD = PRD)
  }
  if(any(methods == 'ARC')){
    ARC <- (1/(year.t2-year.t1))*log(A.t2/A.t1) * 100
    # Annual rate of change from Puyravaud 2004 (also known as instantaneous rate of change)
    out <- cbind(out, ARC = ARC)
  }
  return (out)
}

#' Future Area Estimate
#'
#' `futureAreaEstimate` is now deprecated, please use
#' `extrapolateEstimate` instead
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
#'  \item Future area as estimated with annual rate of change (ARC)
#'  }
#' @author Nicholas Murray \email{murr.nick@@gmail.com}, Calvin Lee
#'   \email{calvinkflee@@gmail.com}
#' @family change_functions
#' @references Bland, L.M., Keith, D.A., Miller, R.M., Murray, N.J. and
#'   Rodriguez, J.P. (eds.) 2016. Guidelines for the application of IUCN Red
#'   List of Ecosystems Categories and Criteria, Version 1.0. Gland,
#'   Switzerland: IUCN. ix + 94pp. Available at the following web site:
#'   <https://iucnrle.org/>
#' @export

futureAreaEstimate <- function(A.t1, year.t1, nYears, ARD = NA, PRD = NA, ARC = NA){
  .Deprecated("extrapolateEstimate", "redlistr")
  extrapolateEstimate(A.t1 = A.t1, year.t1 = year.t1, nYears = nYears,
                      ARD = ARD, PRD = PRD, ARC = ARC)
}

#' Extrapolate Estimate
#'
#' `extrapolateEstimate` uses rates of decline from getDeclineStats
#' to extrapolate estimates to a given time
#'
#' @param A.t1 Area at time t1
#' @param year.t1 Year of time t1
#' @param nYears Number of years since t1 for prediction. Use negative
#' values for backcasting
#' @param ARD Absolute rate of decline
#' @param PRD Proportional rate of decline
#' @param ARC Annual rate of change
#' @return A dataframe with the forecast year, and a combination of:
#' \itemize{
#'  \item Values as extrapolated with absolute rate of decline (ARD)
#'  \item Values as extrapolated with proportional rate of decline (PRD)
#'  \item Values as extrapolated with annual rate of change (ARC)
#'  }
#' @author Nicholas Murray \email{murr.nick@@gmail.com}, Calvin Lee
#'   \email{calvinkflee@@gmail.com}
#' @family change_functions
#' @references Bland, L.M., Keith, D.A., Miller, R.M., Murray, N.J. and
#'   Rodriguez, J.P. (eds.) 2016. Guidelines for the application of IUCN Red
#'   List of Ecosystems Categories and Criteria, Version 1.0. Gland,
#'   Switzerland: IUCN. ix + 94pp. Available at the following web site:
#'   <https://iucnrle.org/>
#' @examples
#' a.r1 <- 23.55
#' a.r2 <- 15.79
#' decline.stats <- getDeclineStats(a.r1, a.r2, year.t1 = 1990, year.t2 = 2012,
#'                                  methods = 'PRD')
#' a.2040.PRD <- extrapolateEstimate(a.r1, a.r2, year.t1 = 1990, nYears = 50,
#'                                   PRD = decline.stats$PRD)
#' @export

extrapolateEstimate <- function(A.t1, year.t1, nYears, ARD = NA, PRD = NA, ARC = NA){
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
    A.ARC.t3 <- A.t1 * exp(ARC/100*nYears)
    if(A.ARC.t3 < 0) A.ARC.t3 = 0
    out <- cbind(out, A.ARC.t3 = A.ARC.t3)
  }
  if(all(c(is.na(PRD), is.na(ARD), is.na(ARC)))){
    stop("Please input at least one of 'ARD', 'PRD', or 'ARC'.")
  }
  return(out)
}

#' Sequential extrapolation estimate
#'
#' `sequentialExtrapolate` uses rates of decline from getDeclineStats and
#' generates a sequence of estimates at regular time-steps. Useful for
#' generating a sequence for plotting graphs.
#'
#' @inheritParams extrapolateEstimate
#'
#' @return A dataframe with the forecast year, and a combination of:
#' \itemize{
#'  \item Sequence of values as extrapolated with absolute rate of decline (ARD)
#'  \item Sequence of values as extrapolated with proportional rate of decline (PRD)
#'  \item Sequence of values as extrapolated with annual rate of change (ARC)
#'  }
#' @author Calvin Lee \email{calvinkflee@@gmail.com}
#' @family change_functions
#' @references Bland, L.M., Keith, D.A., Miller, R.M., Murray, N.J. and
#'   Rodriguez, J.P. (eds.) 2016. Guidelines for the application of IUCN Red
#'   List of Ecosystems Categories and Criteria, Version 1.0. Gland,
#'   Switzerland: IUCN. ix + 94pp. Available at the following web site:
#'   <https://iucnrle.org/>
#' @examples
#' a.r1 <- 23.55
#' a.r2 <- 15.79
#' decline.stats <- getDeclineStats(a.r1, a.r2, year.t1 = 1990, year.t2 = 2012,
#'                                  methods = 'PRD')
#' a.2040.PRD.seq <- sequentialExtrapolate(a.r1, a.r2, year.t1 = 1990, nYears = 50,
#'                                         PRD = decline.stats$PRD)
#' @export

sequentialExtrapolate <- function(A.t1, year.t1, nYears, ARD = NA, PRD = NA, ARC = NA){
  if(all(c(is.na(PRD), is.na(ARD), is.na(ARC)))){
    stop("Please input at least one of 'ARD', 'PRD', or 'ARC'.")
  }
  ARD_seq <- vector()
  PRD_seq <- vector()
  ARC_seq <- vector()
  for(i in 0:nYears){
    estimate <- extrapolateEstimate(A.t1,
                                    year.t1,
                                    nYears = i,
                                    ARD = ARD,
                                    PRD = PRD,
                                    ARC = ARC)

    ARD_seq <- c(ARD_seq, estimate$A.ARD.t3)
    PRD_seq <- c(PRD_seq, estimate$A.PRD.t3)
    ARC_seq <- c(ARC_seq, estimate$A.ARC.t3)
  }

  years <- seq(year.t1, sum(year.t1, nYears))

  # Dealing with empty vectors
  if(length(ARD_seq) == 0) ARD_seq <- NA
  if(length(PRD_seq) == 0) PRD_seq <- NA
  if(length(ARC_seq) == 0) ARC_seq <- NA

  out_df <- data.frame(years = years, ARD = ARD_seq,
                       PRD = PRD_seq, ARC = ARC_seq)
  return(out_df)
}
