#' Base function to compute AOO with grid uncertainty
#'
#' \code{gridUncertaintyBase} determines the number of area of occupancy (AOO)
#' grid cells occupied by a species or ecosystem. It is the base function which
#' is used by \code{gridUncertainty} and \code{gridUncertaintySimulation}
#' @inheritParams createGrid
#' @param split Specifies the number of ways to split the grid in ONE axis.
#' @param min.percent.rule Logical. If \code{TRUE}, a minimum area threshold
#'   must be passed before a grid is counted as an AOO grid.
#' @param percent Numeric. The minimum percent to be applied as a threshold for
#'   the \code{min.percent.rule}.
#' @return List containing vector of length split*split of calculated AOO for
#'   each shift position, a list of summary statistics for the vector, a list of
#'   grids which generated the smallest AOO as RasterLayers, and the movements
#'   which resulted in the grids with the smallest AOO.
#' @author Nicholas Murray \email{murr.nick@@gmail.com}, Calvin Lee
#'   \email{calvinkflee@@gmail.com}
#' @family gridUncertainty functions
#' @seealso \code{\link{createGrid}} \code{\link{getAOO}}
#'   \code{\link{getAOOSilent}}
#' @references Bland, L.M., Keith, D.A., Miller, R.M., Murray, N.J. and
#'   Rodriguez, J.P. (eds.) 2016. Guidelines for the application of IUCN Red
#'   List of Ecosystems Categories and Criteria, Version 1.0. Gland,
#'   Switzerland: IUCN. ix + 94pp. Available at the following web site:
#'   \url{iucnrle.org/}
#' @examples
#' x <- gridUncertaintyBase(r1, 10000, 3, F)
#' x$AOO.list # vector of length 9 of calculated AOO for each grid position
#' x$stats # dataframe returning the number of shifts performed, summary statistics
#' for the calculated AOOs, and the shift values which created the grid
#' position(s) with the smallest AOO.
#' x$min.AOO.grid.list # list of RasterLayers of the grids which all return the
#' minimum AOO.
#' x$min.AOO.shifts # dataframe containing information of the index of the shift
#' which created the grids with the smallest AOO and the x and y shifts for them.
#' @export
gridUncertaintyBase <- function(ecosystem.data, grid.size,
                                splits, min.percent.rule = TRUE, percent = 1){
  grid <- createGrid(ecosystem.data, grid.size)
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
                     min.percent.rule = min.percent.rule,
                     percent = percent)
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

#' Function to compute AOO with grid uncertainty
#'
#' \code{gridUncertainty} determines the number of area of occupancy (AOO)
#' grid cells occupied by a species or ecosystem. It will only stop when the AOO
#' calculated does not improve after a set number of splits
#' @inheritParams createGrid
#' @param n.AOO.improvement Specifies the minimum number of rounds the
#'   calculated AOO is not improved before stopping the function.
#' @param min.percent.rule Logical. If \code{TRUE}, a minimum area threshold
#'   must be passed before a grid is counted as an AOO grid.
#' @param percent Numeric. The minimum percent to be applied as a threshold for
#'   the \code{min.percent.rule}
#' @return Data frame of results showing the minimum and maximum AOO calculated
#'   for each grid shift scenario
#' @author Calvin Lee \email{calvinkflee@@gmail.com}
#' @family gridUncertainty functions
#' @references Bland, L.M., Keith, D.A., Miller, R.M., Murray, N.J. and
#'   Rodriguez, J.P. (eds.) 2016. Guidelines for the application of IUCN Red
#'   List of Ecosystems Categories and Criteria, Version 1.0. Gland,
#'   Switzerland: IUCN. ix + 94pp. Available at the following web site:
#'   \url{iucnrle.org/}
#' @examples
#' AOO.df <- gridUncertainty(r, 10000, 4, T, 1)
#' AOO <- min(AOO.df$min.AOO)
#' @export
gridUncertainty <- function(ecosystem.data, grid.size, n.AOO.improvement, min.percent.rule, percent){
  out.df <- data.frame('n.splits' = 0,
                       'min.AOO' = 0)
  for(i in 1:n.AOO.improvement){
    out.df[i, 1] <- i
    results <- gridUncertaintyBase(ecosystem.data = ecosystem.data, grid.size = grid.size, splits = i, min.percent.rule = min.percent.rule, percent = percent)
    out.df[i, 2] <- results$stats$min.AOO
  }
  for(i in n.AOO.improvement+1:1000){ #arbitrary large number
    out.df[i, 1] <- i
    results <- gridUncertaintyBase(ecosystem.data = ecosystem.data, grid.size = grid.size, splits = i,
                               min.percent.rule = min.percent.rule, percent = percent)
    out.df[i, 2] <- results$stats$min.AOO
    for (j in 1:(n.AOO.improvement-1)){
      logic.list <- out.df[i-n.AOO.improvement, 2] <= out.df[(i-n.AOO.improvement+j), 2]
    }
    if (all(logic.list)) break # checking if the min.AOO decreases
  }
  return(out.df)
}


#' Function to investigate behaviour of AOO under various splits
#'
#' \code{gridUncertaintySimulation} returns the number of area of occupancy
#' (AOO) grid cells occupied by a species or ecosystem in incremental splits
#' using \code{gridUncertaintyBase}.
#' @inheritParams createGrid
#' @param simulations Specifies the maximum number of splits to be performed on
#'   the generated grid
#' @param min.percent.rule Logical. If \code{TRUE}, a minimum area threshold
#'   must be passed before a grid is counted as an AOO grid.
#' @param percent Numeric. The minimum percent to be applied as a threshold for
#'   the \code{min.percent.rule}
#' @return Data frame of results showing the minimum and maximum AOO calculated
#'   for each grid shift scenario
#' @author Calvin Lee \email{calvinkflee@@gmail.com}
#' @family gridUncertainty functions
#' @references Bland, L.M., Keith, D.A., Miller, R.M., Murray, N.J. and
#'   Rodriguez, J.P. (eds.) 2016. Guidelines for the application of IUCN Red
#'   List of Ecosystems Categories and Criteria, Version 1.0. Gland,
#'   Switzerland: IUCN. ix + 94pp. Available at the following web site:
#'   \url{iucnrle.org/}
#' @examples
#' AOO.df <- gridUncertaintySimulation(r, 10000, 10, T, 1)
#' plot(AOO.df$n.splits, AOO.df$min.AOO,
#'      type = 'l', col = 'red', ylim = c(0, 10), xlab = 'Number of splits', ylab = 'Minimum AOO')
#' lines(AOO.df$n.splits, AOO.df$max.AOO, col = 'blue')
#' @export
gridUncertaintySimulation <- function(ecosystem.data, grid.size,
                                      simulations, min.percent.rule, percent){
  out.df <- data.frame('n.splits' = rep(0, simulations),
                       'min.AOO' = rep(0, simulations),
                       'max.AOO' = rep(0, simulations))
  for(i in 1:simulations){
    out.df[i, 1] <- i
    results <- gridUncertaintyBase(ecosystem.data = ecosystem.data,
                               grid.size = grid.size, splits = i,
                               min.percent.rule = min.percent.rule, percent = percent)
    out.df[i, 2] <- results$stats$min.AOO
    out.df[i, 3] <- results$stats$max.AOO
  }
  return(out.df)
}
