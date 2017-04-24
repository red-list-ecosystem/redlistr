#' Compute Area of Occupancy (AOO)
#'
#' \code{getAOO} determines the number of area of occupancy (AOO) grid cells
#' occupied by a species or ecosystem. It includes capability for specifying
#' whether at least one percent of the grid cell needs to be occupied before it
#' is counted in the AOO. This functionality is important for assessing the IUCN
#' Red List of Ecosystems Criteria B.
#'
#' @inheritParams makeAOOGrid
#' @return The number of grid cells occupied by the ecosystem or species
#' @author Nicholas Murray \email{murr.nick@@gmail.com}
#' @seealso \code{\link{createGrid}}
#' @references Bland, L.M., Keith, D.A., Miller, R.M., Murray, N.J. and
#'   Rodriguez, J.P. (eds.) 2016. Guidelines for the application of IUCN Red
#'   List of Ecosystems Categories and Criteria, Version 1.0. Gland,
#'   Switzerland: IUCN. ix + 94pp. Available at the following web site:
#'   \url{iucnrle.org/}
#' @examples
#' n = createGrid(r1, 10000)
#' plot (n)
#' plot (r1, add = TRUE)
#' AOO = getAOO(r1, n, one.percent.rule = F)
#' AOO # number of grid cells occupied by an ecosystem or species

getAOO <- function (ecosystem.data, grid.size, one.percent.rule = TRUE) {
  # Computes the number of 10x10km grid cells that are >1% covered by an ecosystem
    AOO.number = length(makeAOOGrid(ecosystem.data, grid.size, one.percent.rule))
  return(AOO.number)
}
