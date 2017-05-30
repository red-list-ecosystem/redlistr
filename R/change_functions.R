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
#' @seealso
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

#' Area Change from Raster.
#'
#' \code{getAreaChangefromRast} reports the difference in area of two raster
#' objects.
#'
#' @param x Raster object of distribution
#' @param y Raster object of distribution
#' @return Returns the difference in area of two raster objects in km2
#' @author Nicholas Murray \email{murr.nick@@gmail.com}
#' @family Change functions
#' @examples
#' getAreaChangefromRast(r1, r2) # distribution rasters

getAreaChangefromRast <- function (x, y){
  a1 <- getArea(x)
  a2 <- getArea(y)
  a.dif <- a1 - a2
  return(a.dif)
}

#' Area Change from Polygons.
#'
#' \code{getAreaChangefromShp} reports the difference in area of two polygon
#' datasets
#' @param x Polygon of distribution
#' @param y Polygon of distribution
#' @return Returns the difference in area of two polygon objects in km2
#' @author Nicholas Murray \email{murr.nick@@gmail.com}
#' @family Change functions
#' @examples
#' getAreaChangefromShp(p1, p2) # distribution rasters

getAreaChangefromShp <- function(p1, p2){
  a1 <- gArea(p1)
  a2 <- gArea(p2)
  a.dif.m2 <- a1 - a2
  a.dif.km2 <- a.dif.m2/1000000
  return (a.dif.km2)
}


#' Annual Rate of Change.
#'
#' \code{getARC} calculates the Annual Rate of Change (ARC) given two areas and
#' two time points. Further information on the approach and assumptions of this
#' exponential decay model is available in Puyravaud (2003). Annual rate of
#' change is also known as instantaneous rate of change.
#'
#' @param A.t1 Area at time t1
#' @param year.t1 Year of time t1
#' @param A.t2 Area at time t2
#' @param year.t2 Year of time t2
#' @return The annual rate of change (ARC) between the two time periods
#' @author Nicholas Murray \email{murr.nick@@gmail.com}, Calvin Lee
#'   \email{calvinkflee@@gmail.com}
#' @family Change functions
#' @references Puyravaud, J.-P. 2003. Standardizing the calculation of the
#' annual rate of deforestation. Forest Ecology and Management, 177, 593-596.
#' @examples
#' a.r1 <- getArea(r1) # a distribution raster
#' a.r2 <- getArea(r2) # a distribution raster
#' ARC <- getARC(a.r1, a.r2, year.t1 = 1990, year.t2 = 2012)

getARC <- function (A.t1, A.t2, year.t1, year.t2){
  # Annual rate of change from Puyravaud 2004. Also known as instantaneous rate of change.
  ARC <- (1/(year.t2-year.t1))*log(A.t2/A.t1)
  return (ARC)
}

#' Absolute Rate of Decline.
#'
#' \code{getPRD} calculates the Proportional Rate of Decline (PRD) given two
#' areas and two time points. Further information on the approach and
#' assumptions of this model is available in Keith et al (2003). PRD is
#' sometimes known as trajectory (eg. Waycott et al 2009).
#'
#' @inheritParams getARC
#' @return Returns the proportional rate of decline (PRD) between the two time
#' periods
#' @author Nicholas Murray \email{murr.nick@@gmail.com}
#' @family Change functions
#' @references Keith, D. A., Orscheg, C., Simpson, C. C., Clarke, P. J., Hughes,
#' L., Kennelly, S. J., Major, R. E., Soderquist, T. R., Wilson, A. L. &
#' Bedward, M. 2009. A new approach and case study for estimating extent and
#' rates of habitat loss for ecological communities. Biological Conservation,
#' 142, 1469-1479.
#' @references Bland, L.M., Keith, D.A., Miller, R.M., Murray, N.J. and
#' Rodriguez, J.P. (eds.) 2016. Guidelines for the application of IUCN Red
#' List of Ecosystems Categories and Criteria, Version 1.0. Gland, Switzerland:
#' IUCN. ix + 94pp. Available at the following web site: \url{iucnrle.org/}
#' @examples
#' a.r1 <- getArea(r1) # a distribution raster
#' a.r2 <- getArea(r2) # a distribution raster
#' PRD <- getPRD(a.r1, a.r2, year.t1 = 1990, year.t2 = 2012)

getPRD <- function (A.t1, A.t2, year.t1, year.t2){
  # Proportional rate of change (also known as trajectory (r))
  PRD <- 100 * (1-(A.t2/A.t1)^(1/(year.t2-year.t1)))
  return (PRD)
}

#' Absolute Rate of Decline.
#'
#' \code{getARD} calculates the Absolute Rate of Decline (ARD) given two areas
#' and two time points. Further information on the approach and assumptions of
#' this model is available in Keith et al (2003). ARD is sometimes also known as
#' Annual Change (R).
#'
#' @inheritParams getARC
#' @return An area representing The absolute rate of decline (ARD) between the
#'   two time periods
#' @author Nicholas Murray \email{murr.nick@@gmail.com}
#' @family Change functions
#' @references Keith, D. A., Orscheg, C., Simpson, C. C., Clarke, P. J., Hughes,
#'   L., Kennelly, S. J., Major, R. E., Soderquist, T. R., Wilson, A. L. &
#'   Bedward, M. 2009. A new approach and case study for estimating extent and
#'   rates of habitat loss for ecological communities. Biological Conservation,
#'   142, 1469-1479.
#' @references Puyravaud, J.-P. 2003. Standardizing the calculation of the
#'   annual rate of deforestation. Forest Ecology and Management, 177, 593-596.
#' @references Bland, L.M., Keith, D.A., Miller, R.M., Murray, N.J. and
#'   Rodríguez, J.P. (eds.) 2016. Guidelines for the application of IUCN Red
#'   List of Ecosystems Categories and Criteria, Version 1.0. Gland,
#'   Switzerland: IUCN. ix + 94pp. Available at the following web site:
#'   \url{iucnrle.org/}
#' @examples
#' a.r1 <- getArea(r1) # a distribution raster
#' a.r2 <- getArea(r2) # a distribution raster
#' ARD <- getARD(a.r1, a.r2, year.t1 = 1990, year.t2 = 2012)
#' ARD

getARD <- function (A.t1, A.t2, year.t1, year.t2){
  # Absolute Rate of Change (also known as Annual Change(R)) in Puyrvaud
  ARD <- (A.t2-A.t1)/(year.t2-year.t1)
  ARD <- -ARD # make it a positive number to be consistent with Keith et al 2009
  return (ARD)
}

#' Percent change
#'
#' \code{getPercentLost} reports the percentage change between two numbers (such
#' as areas)
#'
#' @param A.t1 Area at t1
#' @param A.t2 Area at t2
#' @return The percentage change  between these two areas
#' @author Nicholas Murray \email{murr.nick@@gmail.com}
#' @family Change functions
#' @examples
#' getAreaChange(100,80)

getPercentLost <- function(A.t1, A.t2){
  pc.lost <- ((A.t1 - A.t2)/A.t1)*100
  return (pc.lost)
}

#' Change statistics.
#'
#' \code{getDeclineStats} calculates the Proportional Rate of Decline (PRD),
#' Absolute Rate of Decline (ARD) and Absolute Rate of Change (ARC), given two
#' areas at two points in time. Also provides an estimate of area change and
#' percentage lost between the two time periods.
#'
#' @inheritParams getARC
#' @return A dataframe with Proportional Rate of Decline (PRD), Absolute Rate of
#'   Decline (ARD), Absolute Rate of Change (ARC), the two time periods, an
#'   estimate of area change and the percentage lost between the two time
#'   periods.
#' @author Nicholas Murray \email{murr.nick@@gmail.com}, Calvin Lee
#'   \email{calvinkflee@@gmail.com}
#' @family Change functions
#' @references Bland, L.M., Keith, D.A., Miller, R.M., Murray, N.J. and
#'   Rodriguez, J.P. (eds.) 2016. Guidelines for the application of IUCN Red
#'   List of Ecosystems Categories and Criteria, Version 1.0. Gland,
#'   Switzerland: IUCN. ix + 94pp. Available at the following web site:
#'   \url{iucnrle.org/}
#' @examples
#' a.r1 <- getArea(r1) # a distribution raster
#' a.r2 <- getArea(r2) # a distribution raster
#' RLEstats <- getDeclineStats(a.r1, a.r2, year.t1 = 1990, year.t2 = 2012)

getDeclineStats <- function (A.t1, A.t2, year.t1, year.t2){
  # consider raw code rather than a function in here
  ARD <- getARD (A.t1, A.t2, year.t1, year.t2)
  PRD <- getPRD (A.t1, A.t2, year.t1, year.t2)
  ARC <- getARC (A.t1, A.t2, year.t1, year.t2)
  area.change <- A.t1 - A.t2
  pc.lost <- getPercentLost(A.t1,A.t2)
  out <- data.frame(area.t1 = A.t1,
                    area.t2 = A.t2,
                    year.t1 = year.t1,
                    year.t2 = year.t2,
                    abs.rate.decl = ARD,
                    prop.rate.decl = PRD,
                    annual.rate.change = ARC,
                    area.change = area.change,
                    percent.lost = pc.lost)
  return (out)
}