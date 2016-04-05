#' Absolute Rate of Decline.
#'
#' \code{getPRD} calculates the Proportional Rate of Decline (PRD) given two
#' areas and two time points. Further information on the approach and
#' assumptions of this model is available in Keith et al (2003). PRD is
#' sometimes known as trajectory (eg. Waycott et al 2009).
#'
#' @param A.t1 Area at time t1
#' @param year.t1 Year of time t1
#' @param A.t2 Area at time t2
#' @param year.t2 Year of time t2
#' @return Returns the proportional rate of decline (PRD) between the two time
#' periods
#' @author Nicholas Murray \email{murr.nick@@gmail.com}
#' @seealso \code{\link{getArea}}, \code{\link{getPRD}}, \code{\link{getARD}},
#' \code{\link{getARC}}
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
#' a.r1 = getArea(r1) # a distribution raster
#' a.r2 = getArea(r2) # a distribution raster
#' PRD = getPRD(a.r1, a.r2, year.t1 = 1990, year.t2 = 2012)
#' PRD

getPRD <- function (A.t1, A.t2, year.t1, year.t2){
  # Proportional rate of change (also known as trajectory (r))
  PRD <- 100 * (1-(A.t2/A.t1)^(1/(year.t2-year.t1)))
  return (PRD)
}

# getPRD2 <- function (A.t1, A.t2, year.t1, year.t2){
#   PRD2 <- 100 * (1-(exp(log(A.t2/A.t1)/(year.t2-year.t1))))
#   return (PRD2)
# }
