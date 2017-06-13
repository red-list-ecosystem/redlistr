#' Base function to compute AOO with grid uncertainty systematically
#'
#' \code{gridUncertaintyBase} helps determine the minimum number of area of
#' occupancy (AOO) grid cells occupied by a species or ecosystem. It varies the
#' location of the AOO grid by shifting in systematically in both x- and y-
#' axes, returning summary statistics for the range of AOOs calculated, and the
#' RasterLayer(s) containing the grids with the minimum AOO. It is the base
#' function which is used by \code{gridUncertainty} and
#' \code{gridUncertaintySimulation}
#' @inheritParams createGrid
#' @param split Specifies the number of ways to split the grid in ONE axis.
#' @param min.percent.rule Logical. If \code{TRUE}, a minimum area threshold
#'   must be passed before a grid is counted as an AOO grid.
#' @param percent Numeric. The minimum percent to be applied as a threshold for
#'   the \code{min.percent.rule}.
#' @return List containing the following:
#' \itemize{
#'  \item Vector of length split*split of calculated AOO for each shifted grid
#'  \item Data frame of summary statistics for the results
#'  \item Data frame showing the distance shifted in x and y directions used to
#'  create the AOO grid(s) which return the smallest AOO
#'  \item List of RasterLayer(s) containing the AOO grid(s) which return the
#'  smallest AOO
#' }
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
#' x$min.AOO.rasters # list of RasterLayers of the grids which all return the
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
              min.AOO.rasters = grid.shifted.list[min.AOO.index],
              min.AOO.shifts = shift.grid[min.AOO.index, ]))
}

#' Manual function to compute AOO with grid uncertainty randomly
#'
#' \code{gridUncertaintyRandomManual} helps determine the minimum number of area of
#' occupancy (AOO) grid cells occupied by a species or ecosystem. It varies the
#' location of the AOO grid by shifting in randomly in both x- and y-
#' axes, returning summary statistics for the range of AOOs calculated, and the
#' RasterLayer(s) containing the grids with the minimum AOO. Requires manual
#' number of simulations input
#' @inheritParams createGrid
#' @param n.sim Specifies the number of random grids to be created and tested
#' @param min.percent.rule Logical. If \code{TRUE}, a minimum area threshold
#'   must be passed before a grid is counted as an AOO grid.
#' @param percent Numeric. The minimum percent to be applied as a threshold for
#'   the \code{min.percent.rule}.
#' @return List containing the following:
#' \itemize{
#'  \item Data frame of summary statistics for the results
#'  \item Data frame showing the distance shifted in x and y directions used to
#'  create the AOO grid(s) and their associated AOOs
#'  \item List of RasterLayer(s) containing the AOO grid(s) which return the
#'  smallest AOO
#' }
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
#' x <- gridUncertaintyRandom(sample.mang, 10000, 50, T, 1)
#' x$shift.df # vector of length 50 of calculated AOO  and random movement for
#' each randomly generated grid
#' x$AOO.stats # data frame returning the number of shifts performed and the
#' summary statistics for the calculated AOOs.
#' x$min.AOO.rasters # list of RasterLayers of the grids which all return the
#' minimum AOO.
#' @export

gridUncertaintyRandomManual <- function(ecosystem.data, grid.size,
                                      n.sim = 10, min.percent.rule = T, percent = 1){
  grid <- createGrid(ecosystem.data = ecosystem.data, grid.size = grid.size)
  results.df <- data.frame(sim.no = integer(),
                           x.shift = integer(),
                           y.shift = integer(),
                           dist.move = double(),
                           AOO = integer(),
                           stringsAsFactors = F)
  output.rasters <- list()
  for (i in 1:n.sim){
    x.shift <- sample(-grid.size:grid.size, 1)
    y.shift <- sample(-grid.size:grid.size, 1)
    dist.move <- sqrt((x.shift^2)+(y.shift^2)) # Total distance moved using Pythagoras
    shifted.grid <- shift(grid, x = x.shift, y = y.shift)
    AOO <- getAOOSilent(ecosystem.data = ecosystem.data,
                        grid = shifted.grid, min.percent.rule = min.percent.rule,
                        percent = percent) # get the AOO for each sampled grid
    sim.df <- data.frame(sim.no = i,
                         x.shift = x.shift,
                         y.shift = y.shift,
                         dist.move = dist.move,
                         AOO = AOO,
                         stringsAsFactors = F)
    results.df <- rbind(results.df, sim.df)
    output.rasters[[i]] <- shifted.grid
  }
  min.AOO <- min(results.df$AOO)
  min.AOO.index <- which(results.df$AOO == min.AOO)
  stat.df <- data.frame(n.sims = n.sim,
                        mean.AOO = mean(results.df$AOO),
                        var.AOO = var(results.df$AOO),
                        min.AOO = min(results.df$AOO),
                        max.AOO = max(results.df$AOO))
  min.AOO.rasters <- output.rasters[min.AOO.index]
  names(min.AOO.rasters) <- min.AOO.index

  return(list(AOO.stats = stat.df,
              shift.df = results.df,
              min.AOO.rasters = min.AOO.rasters))
}

#' Function to compute AOO with grid uncertainty systematically with stopping rule
#'
#' \code{gridUncertainty} determines the number of area of occupancy (AOO) grid
#' cells occupied by a species or ecosystem systematically. It will only stop
#' when the AOO calculated does not improve (decrease) after a set number of
#' split scenarios.
#' @inheritParams createGrid
#' @param n.AOO.improvement Specifies the minimum number of rounds the
#'   calculated AOO is not improved before stopping the function.
#' @param min.percent.rule Logical. If \code{TRUE}, a minimum area threshold
#'   must be passed before a grid is counted as an AOO grid.
#' @param percent Numeric. The minimum percent to be applied as a threshold for
#'   the \code{min.percent.rule}
#' @return A list containing the following:
#' \itemize{
#'  \item Data frame of results showing the minimum AOO calculated for each
#'  shift scenario
#'  \item List of rasterLayers which were used to generate the
#'  minimum AOO specified for each shift scenario
#' }
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

gridUncertainty <- function(ecosystem.data, grid.size, n.AOO.improvement, min.percent.rule = T, percent = 1){
  out.df <- data.frame('n.splits' = 0,
                       'min.AOO' = 0)
  min.rasters <- list()
  for(i in 1:n.AOO.improvement){
    out.df[i, 1] <- i
    results <- gridUncertaintyBase(ecosystem.data = ecosystem.data, grid.size = grid.size, splits = i,
                                   min.percent.rule = min.percent.rule, percent = percent)
    out.df[i, 2] <- results$stats$min.AOO
    min.rasters[[i]] <- results$min.AOO.grid.list
  }
  for(i in n.AOO.improvement+1:1000){ #arbitrary large number
    out.df[i, 1] <- i
    results <- gridUncertaintyBase(ecosystem.data = ecosystem.data, grid.size = grid.size, splits = i,
                                   min.percent.rule = min.percent.rule, percent = percent)
    out.df[i, 2] <- results$stats$min.AOO
    min.rasters[[i]] <- results$min.AOO.grid.list
    logic.test <- vector()
    for (j in 1:(n.AOO.improvement-1)){
      logic.test <- c(logic.test, out.df[i-n.AOO.improvement, 2] <= out.df[(i-n.AOO.improvement+j), 2])
    }
    if (all(logic.test)) break # checking if the min.AOO decreases
  }
  results <- list('min.AOO.df' = out.df,
                  'min.AOO.rasters' = min.rasters)
  return(results)
}

## TODO: add slight movement to each grid shift to reduce caclulating repeated grids?

#' Function to investigate behaviour of AOO under various split scenarios
#'
#' \code{gridUncertaintySimulation} returns the maximum and minimum number of
#' area of occupancy (AOO) grid cells occupied by a species or ecosystem in
#' incremental splits using \code{gridUncertaintyBase}.
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
#' plot(AOO.df$n.splits, AOO.df$min.AOO, type = 'l', col = 'red',
#'      ylim = c(0, 10), xlab = 'Number of splits', ylab = 'Minimum AOO')
#' lines(AOO.df$n.splits, AOO.df$max.AOO, col = 'blue')
#' @export

gridUncertaintySimulation <- function(ecosystem.data, grid.size,
                                      simulations, min.percent.rule = T, percent = 1){
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
