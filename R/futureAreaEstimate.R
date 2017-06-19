#' Future Area Estimate
#'
#' \code{futureAreaEstimate} calculates the expected area of a distribution at a
#' future date using known rates of decline.
#'
#' @param A.t1 Area at time t1
#' @param year.t1 Year of time t1
#' @param PRD Proportional rate of decline
#' @param ARD Absolute rate of decline
#' @param ARC Annual rate of change
#' @param nYears Number of years since t1 for area prediction
#' @return A dataframe listing \item{area.t1}{area at time t1}
#'   \item{year.t1}{year of t1.} \item{prop.rate.decl}{proportional rate of
#'   decline.} \item{abs.rate.decl}{absolute rate of decline.}
#'   \item{annual.rate.change}{annual rate of change.} \item{forecast.year}{year
#'   for which future area estimate is made.} \item{forecast.area.prd}{future
#'   area as estimated with proportional rate of decline.}
#'   \item{forecast.area.arc}{future area as estimated with annual rate of
#'   change .} \item{forecast.area.ard}{future area as estimated with absolute
#'   rate of decline.}
#' @author Nicholas Murray \email{murr.nick@@gmail.com}, Calvin Lee
#'   \email{calvinkflee@@gmail.com}
#' @family change_functions
#' @references Bland, L.M., Keith, D.A., Miller, R.M., Murray, N.J. and
#'   Rodriguez, J.P. (eds.) 2016. Guidelines for the application of IUCN Red
#'   List of Ecosystems Categories and Criteria, Version 1.0. Gland,
#'   Switzerland: IUCN. ix + 94pp. Available at the following web site:
#'   \url{iucnrle.org/}
#' @examples
#' a.r1 = getArea(r1) # a distribution raster
#' a.r2 = getArea(r2) # a distribution raster
#' PRD = getPRD(a.r1, a.r2, year.t1 = 1990, year.t2 = 2012)
#' ARD = getARD(a.r1, a.r2, year.t1 = 1990, year.t2 = 2012)
#' ARC = getARC(a.r1, a.r2, year.t1 = 1990, year.t2 = 2012)
#' area2050 <- futureAreaEstimate(A.t1 = a.r1, 2000, PRD, ARD, ARC, nYears = 50)
#' @export

futureAreaEstimate <- function(A.t1, year.t1, PRD, ARD, ARC, nYears){
  A.PRD.t3 <- A.t1 * (1 -(PRD/100))^nYears
  A.ARC.t3 <- A.t1 * (1 + ARC)^nYears
  A.ARD.t3 <- A.t1 - (ARD*nYears)
  if (A.PRD.t3 < 0) A.PRD.t3 = 0
  if (A.ARC.t3 < 0) A.ARC.t3 = 0
  if (A.ARD.t3 < 0) A.ARD.t3 = 0
  y.t3 <- year.t1+nYears
  out <- data.frame(area.t1 = A.t1,
                    year.t1 = year.t1,
                    prop.rate.decl = PRD,
                    abs.rate.decl = ARD,
                    annual.rate.change = ARC,
                    forecast.year = y.t3,
                    forecast.area.prd = A.PRD.t3,
                    forecast.area.arc = A.ARC.t3,
                    forecast.area.ard = A.ARD.t3)
  return(out)
}
