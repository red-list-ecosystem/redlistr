#' Compute Area of Occupancy (AOO) accounting for grid uncertainty
#'
#' \code{gridUncertainty} determines the number of area of occupancy (AOO) grid
#' cells occupied by a species or ecosystem. The key difference with
#' \code{getAOO} is that it accounts for the impact of the location of the AOO
#' grid by systematically moving the grid and recomputing AOO at each step.
#'
#' @param x Raster object of an ecosystem or species distribution
#' @param grid Raster object of a regular grid suitable for assessing AOO
#' @param split Specifies the number of ways to split the grid in ONE axis.
#' @param min.percent.rule Logical. If \code{TRUE}, a minimum area threshold
#'   must be passed before a grid is counted as an AOO grid.
#' @param percent Numeric. The minimum percent to be applied as a threshold for
#'   the \code{min.percent.rule}
#' @return List containing vector of length split*split of calculated AOO for
#'   each shift position, a list of summary statistics for the vector, a list of
#'   grids which generated the smallest AOO as RasterLayers, and the movements
#'   which resulted in the grids with the smallest AOO.
#' @author Nicholas Murray \email{murr.nick@@gmail.com}, Calvin Lee
#'   \email{calvinkflee@@gmail.com}
#' @seealso \code{\link{createGrid}} \code{\link{getAOO}}
#'   \code{\link{getAOOSilent}}
#' @references Bland, L.M., Keith, D.A., Miller, R.M., Murray, N.J. and
#'   Rodriguez, J.P. (eds.) 2016. Guidelines for the application of IUCN Red
#'   List of Ecosystems Categories and Criteria, Version 1.0. Gland,
#'   Switzerland: IUCN. ix + 94pp. Available at the following web site:
#'   \url{iucnrle.org/}
#' @examples
#' x <- gridUncertainty(r1, 10000, 3, F)
#' x$AOO.list # vector of length 9 of calculated AOO for each grid position
#' x$stats # dataframe returning the number of shifts performed, summary statistics
#' for the calculated AOOs, and the shift values which created the grid
#' position(s) with the smallest AOO.
#' x$min.AOO.grid.list # list of RasterLayers of the grids which all return the
#' minimum AOO.
#' x$min.AOO.shifts # dataframe containing information of the index of the shift
#' which created the grids with the smallest AOO and the x and y shifts for them.


gridUncertainty <- function(ecosystem.data, grid.size, split, one.percent.rule = TRUE, percent){
  grid <- createGrid(ecosystem.data, grid.size)
  results.df <- data.frame(sim.no = integer(),
                           x.shift = integer(),
                           y.shift = integer(),
                           dist.move = double(),
                           AOO = integer(),
                           stringsAsFactors = FALSE)
  intervals <- grid.size/splits

  x.shift <- seq(0, grid.size, (intervals))
  x.shift <- x.shift[1:length(x.shift)-1] # Removing the final grid which is identical to first
  y.shift <- seq(0, grid.size, (intervals))
  y.shift <- y.shift[1:length(y.shift)-1]
  shift.grid <- expand.grid(x.shift = x.shift, y.shift = y.shift) # Create the movement grid

  grid.shifted.list <- apply(shift.grid, 1, function(gridList){
    # Pull out the values for each shift
    current.xshift <- gridList["x.shift"]
    current.yshift <- gridList["y.shift"]
    grid.shift <- shift(grid, x = current.xshift, y = current.yshift)
    return(grid.shift)
  })

  AOO.list <- sapply(grid.shifted.list, getAOOSilent, # List of AOO for each scenario
                     ecosystem.data = ecosystem.data,
                     one.percent.rule = one.percent.rule, percent = percent)
  min.AOO <- min(AOO.list)
  min.AOO.index <- which(AOO.list==min.AOO)


  message(paste("Minimum AOO (", min.AOO, ") is obtained when the grid is shifted in x by",
                shift.grid[min.AOO.index, 1], "meters and in y by",
                shift.grid[min.AOO.index, 2], "meters."))


  out.df <- data.frame(n.shifts = length(grid.shifted.list),
                       mean.AOO = mean(AOO.list),
                       var.AOO = var(AOO.list),
                       sd.AOO = sd(AOO.list),
                       min.AOO = min(AOO.list),
                       max.AOO = max(AOO.list))

  hist.breaks <- seq(min(AOO.list), max(AOO.list), 1)
  hist(AOO.list, breaks = hist.breaks, col = "darkred", main = "Histogram: AOO Uncertainty")
  return(list(AOO.list = AOO.list, stats = out.df,
              min.AOO.grid.list = grid.shifted.list[min.AOO.index],
              min.AOO.shifts = shift.grid[min.AOO.index, ]))
}
