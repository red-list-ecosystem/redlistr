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
#' @seealso \code{\link{getArea}}, \code{\link{getPRD}}, \code{\link{getARD}},
#' \code{\link{getARC}}
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
