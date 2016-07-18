#' Change statistics.
#'
#' \code{getDeclineStats} calculates the Proportional Rate of Decline (PRD),
#' Absolute Rate of Decline (ARD) and Absolute Rate of Change (ARC), given two
#' areas at two points in time. Also provides an estimate of area change and
#' percentage lost between the two time periods.
#'
#' @param A.t1 Area at time t1
#' @param year.t1 Year of time t1
#' @param A.t2 Area at time t2
#' @param year.t2 Year of time t2
#' @return Returns a dataframe with Proportional Rate of Decline (PRD),
#' Absolute Rate of Decline (ARD), Absolute Rate of Change (ARC), the two
#' time periods, an estimate of area change and the percentage lost
#' between the two time periods.
#' @author Nicholas Murray \email{murr.nick@@gmail.com}
#' @seealso \code{\link{getArea}}, \code{\link{getPRD}}, \code{\link{getARD}},
#' \code{\link{getARC}}, \code{\link{getAreaChange}}, \code{\link{getPercentLost}}
#' @references Bland, L.M., Keith, D.A., Miller, R.M., Murray, N.J. and
#' Rodriguez, J.P. (eds.) 2016. Guidelines for the application of IUCN Red
#' List of Ecosystems Categories and Criteria, Version 1.0. Gland, Switzerland:
#' IUCN. ix + 94pp. Available at the following web site: \url{iucnrle.org/}
#' @examples
#' a.r1 = getArea(r1) # a distribution raster
#' a.r2 = getArea(r2) # a distribution raster
#' RLEstats = getDeclineStats(a.r1, a.r2, year.t1 = 1990, year.t2 = 2012)
#' RLEstats


getDeclineStats <- function (A.t1, A.t2, year.t1, year.t2){
  # consider raw code rather than a function in here
  ARD <- getARD (A.t1, A.t2, year.t1, year.t2)
  PRD <- getPRD (A.t1, A.t2, year.t1, year.t2)
  ARC <- getARC (A.t1, A.t2, year.t1, year.t2)
  area.change <- getAreaChange (A.t1, A.t2)
  pc.lost <- getPercentLost (A.t1, A.t2)
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
