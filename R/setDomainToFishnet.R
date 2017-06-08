#' Set domain size of ecosystem raster
#'
#' \code{setDomainToFishnet} sets the extent of an input ecosystem raster to the
#' same size as another raster (in this case an AOO grid). All new raster cells
#' are set to to the specified value (probably NA). Its primary use will be for
#' simulation modelling.
#' @param x Raster object of an ecosystem or species distribution
#' @param y Raster object by which to extend the raster extend of x
#' @param value The value of all new cells made during the operation
#' @return A raster object of x with the extent of y with new values specified
#'   by value
#' @author Nicholas Murray \email{murr.nick@@gmail.com}
#' @seealso \code{\link{createGrid}}
#' @references Bland, L.M., Keith, D.A., Miller, R.M., Murray, N.J. and
#'   Rodriguez, J.P. (eds.) 2016. Guidelines for the application of IUCN Red
#'   List of Ecosystems Categories and Criteria, Version 1.0. Gland,
#'   Switzerland: IUCN. ix + 94pp. Available at the following web site:
#'   \url{iucnrle.org/}
#' @examples
#' n = setDomainToFishnet(x,y, value = NA)
#' plot (y)
#' plot (x, add = TRUE, col = "black")
#' plot (n, add = TRUE, col = "blue")

setDomainToFishnet <- function(x, y, value = NA){
  ext <- extent(y)
  r <- extend(x, ext, value = value)
  return(r)
}

