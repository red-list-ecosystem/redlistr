#' Base function to compute AOO with grid uncertainty systematically
#'
#' \code{gridUncertaintyBase} helps determine the minimum number of area of
#' occupancy (AOO) grid cells occupied by a species or ecosystem. It varies the
#' location of the AOO grid by shifting in systematically in both x- and y-
#' axes, adding a small amount of random movement (five percent of the
#' \code{grid.size}) at each point. It then returns summary statistics for the
#' range of AOOs calculated, and the RasterLayer(s) containing the grids with
#' the minimum AOO. It is the base function which is used by
#' \code{gridUncertainty}, \code{gridUncertaintySimulation}, and
#' \code{gridUncertaintyRestricted}
#' @inheritParams createGrid
#' @param splits Specifies the number of ways to split the grid in ONE axis.
#' @param min.percent.rule Logical. If \code{TRUE}, a minimum area threshold
#'   must be passed before a grid is counted as an AOO grid.
#' @param percent Numeric. The minimum percent to be applied as a threshold for
#'   the \code{min.percent.rule}.
#' @param restriction Logical. If \code{TRUE}, allows user to specify areas to
#'   focus where grid search is done. Used in gridUncertaintyRestricted.
#' @param min.grids.shift Dataframe object with two columns (x.shift and
#'   y.shift) specifying the coordinates to restrict the AOO grid placement.
#' @return List containing the following:
#' \itemize{
#'  \item Vector of length split*split of calculated AOO for each shifted grid
#'  \item Data frame of summary statistics for the results
#'  create the AOO grid(s) which return the smallest AOO
#'  \item Data frame of the shift(s) required to create the AOO grid(s) with the
#'  smallest AOO
#' }
#' @author Nicholas Murray \email{murr.nick@@gmail.com}, Calvin Lee
#'   \email{calvinkflee@@gmail.com}
#' @family gridUncertainty functions
#' @seealso \code{\link{createGrid}} \code{\link{getAOOSilent}}
#' @import raster

gridUncertaintyBase <- function(input.data, grid.size,
                                splits, min.percent.rule = FALSE, percent = 1,
                                restriction = FALSE, min.grids.shift){
  grid <- createGrid(input.data, grid.size)
  intervals <- grid.size/splits

  x.shift <- seq(0, grid.size, intervals)
  x.shift <- x.shift[1:length(x.shift)-1] # Removing the final grid which is identical to first
  y.shift <- seq(0, grid.size, intervals)
  y.shift <- y.shift[1:length(y.shift)-1]
  # Create the movement grid with jiggle
  shift.grid <- expand.grid(x.shift = (x.shift + sample(0:(grid.size*0.05), 1)),
                            y.shift = (y.shift + sample(0:(grid.size*0.05), 1)))

  if(restriction){ # Only modift shift.grid when restriction = T
    # apply to get index of the restricted shift.grid we want
    shift.grid.restricted.index <- apply(min.grids.shift, 1, function(grids){
      min.AOO.x <- grids['x.shift']
      x.lims <- c(min.AOO.x - intervals, min.AOO.x + intervals)
      min.AOO.y <- grids['y.shift']
      y.lims <- c(min.AOO.y - intervals, min.AOO.y + intervals)
      index <- which(x.lims[1] < shift.grid$x.shift &
                       shift.grid$ x.shift < x.lims[2] &
                       y.lims[1] < shift.grid$y.shift &
                       shift.grid$y.shift< y.lims[2])
      return(index)
      shift.grid.restricted.index <- unique(unlist(shift.grid.restricted.index))
      shift.grid <- shift.grid[shift.grid.restricted.index, ]
    })
  }
  grid.shifted.list <- apply(shift.grid, 1, function(gridList){
    # Pull out the values for each shift
    current.xshift <- gridList["x.shift"]
    current.yshift <- gridList["y.shift"]
    grid.shift <- shift(grid, x = current.xshift, y = current.yshift)
    return(grid.shift)
  })
  AOO.list <- lapply(grid.shifted.list, getAOOSilent, # List of AOO for each scenario
                     input.data = input.data,
                     min.percent.rule = min.percent.rule,
                     percent = percent)
  AOO.numbers <- list()
  for(i in 1:length(AOO.list)){
    AOO.numbers[[i]] <- AOO.list[[i]]$AOO.number
  }
  AOO.numbers <- unlist(AOO.numbers)
  min.AOO <- min(AOO.numbers)
  min.AOO.index <- which(AOO.numbers==min.AOO)

  out.df <- data.frame(n.shifts = length(grid.shifted.list),
                       min.AOO = min(AOO.numbers),
                       max.AOO = max(AOO.numbers))

  return(list(AOO.numbers = AOO.numbers,
              summary.df = out.df,
              min.AOO.shifts = shift.grid[min.AOO.index, ]))
}

#' Manual function to compute AOO with grid uncertainty randomly
#'
#' \code{gridUncertaintyRandomManual} helps determine the minimum number of area of
#' occupancy (AOO) grid cells occupied by a species or ecosystem. It varies the
#' location of the AOO grid by shifting in randomly in both x- and y-
#' axes, returning summary statistics for the range of AOOs calculated, and the
#' RasterLayer(s) containing the grids with the minimum AOO. Requires manual
#' input for the number of simulations to perform.
#' @inheritParams createGrid
#' @param n.sim Specifies the number of random grids to be created and tested.
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
#' @seealso \code{\link{createGrid}} \code{\link{getAOOSilent}}
#' @import raster

gridUncertaintyRandomManual <- function(input.data, grid.size, n.sim = 10,
                                        min.percent.rule = FALSE, percent = 1){
  grid <- createGrid(input.data = input.data, grid.size = grid.size)
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
    AOO <- getAOOSilent(input.data = input.data,
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
  out.df <- data.frame(n.sims = n.sim,
                        min.AOO = min(results.df$AOO),
                        max.AOO = max(results.df$AOO))
  min.AOO.rasters <- output.rasters[min.AOO.index]
  names(min.AOO.rasters) <- min.AOO.index

  return(list(summary.df = out.df,
              shift.df = results.df,
              min.AOO.rasters = min.AOO.rasters))
}

#' Function to compute AOO with grid uncertainty randomly with stop rule
#'
#' \code{gridUncertaintyRandom} helps determine the minimum number of area of
#' occupancy (AOO) grid cells occupied by a species or ecosystem. It varies the
#' location of the AOO grid by shifting in randomly in both x- and y-
#' axes, returning summary statistics for the range of AOOs calculated, and the
#' RasterLayer(s) containing the grids with the minimum AOO. It automatically
#' stops when the AOO no longer improves after a specified number of rounds.
#' @inheritParams createGrid
#' @param n.AOO.improvement Specifies the minimum number of rounds the
#'   calculated AOO is not improved before stopping the function.
#' @param min.percent.rule Logical. If \code{TRUE}, a minimum area threshold
#'   must be passed before a grid is counted as an AOO grid.
#' @param percent Numeric. The minimum percent to be applied as a threshold for
#'   the \code{min.percent.rule}.
#' @param max.n.rounds Specifies the maximum number of rounds to calculate AOOs.
#'   Generally unused except to limit computation time.
#' @return List containing the following:
#' \itemize{
#'  \item Data frame of summary statistics for the results
#'  \item Data frame showing the distance shifted in x and y directions used to
#'  create the AOO grid(s) and their associated AOOs
#'  \item List of RasterLayer(s) containing the AOO grid(s) which return the
#'  smallest AOO
#' }
#' @author Calvin Lee \email{calvinkflee@@gmail.com}. Nicholas Murray
#'   \email{murr.nick@@gmail.com}
#' @family gridUncertainty functions
#' @seealso \code{\link{createGrid}} \code{\link{getAOOSilent}}
#' @examples
#' crs.UTM55S <- '+proj=utm +zone=55 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs'
#' r1 <- raster(ifelse((volcano<130), NA, 1), crs = crs.UTM55S)
#' extent(r1) <- extent(0, 6100, 0, 8700)
#' x <- gridUncertaintyRandom(r1, grid.size = 10000, n.AOO.improvement = 50,
#'                            min.percent.rule = TRUE, percent = 1)
#' @export
#' @import raster

gridUncertaintyRandom <- function(input.data, grid.size, n.AOO.improvement,
                                  min.percent.rule = FALSE, percent = 1,
                                  max.n.rounds = 1000){
  grid <- createGrid(input.data = input.data, grid.size = grid.size)
  results.df <- data.frame(sim.no = integer(),
                           x.shift = integer(),
                           y.shift = integer(),
                           dist.move = double(),
                           AOO = integer(),
                           stringsAsFactors = F)
  output.rasters <- list()
  for (i in 1:n.AOO.improvement){ # First runs before checking for improvement
    x.shift <- sample(-grid.size:grid.size, 1)
    y.shift <- sample(-grid.size:grid.size, 1)
    dist.move <- sqrt((x.shift^2)+(y.shift^2)) # Total distance moved using Pythagoras
    shifted.grid <- shift(grid, x = x.shift, y = y.shift)
    AOO <- getAOOSilent(input.data = input.data,
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

  for (i in n.AOO.improvement+1:max.n.rounds){
    x.shift <- sample(-grid.size:grid.size, 1)
    y.shift <- sample(-grid.size:grid.size, 1)
    dist.move <- sqrt((x.shift^2)+(y.shift^2)) # Total distance moved using Pythagoras
    shifted.grid <- shift(grid, x = x.shift, y = y.shift)
    AOO <- getAOOSilent(input.data = input.data,
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
    logic.test <- vector()
    for (j in 1:(n.AOO.improvement-1)){
      logic.test <- c(logic.test,
                      results.df[(i-n.AOO.improvement), 'AOO'] <=
                        results.df[(i-n.AOO.improvement+j), 'AOO'])
    }
    if (all(logic.test)) break # Stop the function when AOO no longer decreases
  }

  min.AOO <- min(results.df$AOO)
  min.AOO.index <- which(results.df$AOO == min.AOO)
  out.df <- data.frame(n.sims = max(results.df$sim.no),
                        mean.AOO = mean(results.df$AOO),
                        min.AOO = min(results.df$AOO),
                        max.AOO = max(results.df$AOO))
  min.AOO.rasters <- output.rasters[min.AOO.index]
  names(min.AOO.rasters) <- min.AOO.index

  return(list(summary.df = out.df,
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
#'   the \code{min.percent.rule}.
#' @return A list containing the following:
#' \itemize{
#'  \item Data frame of results showing the minimum AOO calculated for each
#'  shift scenario
#'  \item Single SpatialPolygonsDataFrame containing the AOO grid which would
#'  produce the minimum AOO calculated
#' }
#' @author Calvin Lee \email{calvinkflee@@gmail.com}
#' @family gridUncertainty functions
#' @examples
#' crs.UTM55S <- '+proj=utm +zone=55 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs'
#' r1 <- raster(ifelse((volcano<130), NA, 1), crs = crs.UTM55S)
#' extent(r1) <- extent(0, 6100, 0, 8700)
#' x <- gridUncertainty(r1, grid.size = 10000, n.AOO.improvement = 5,
#'                            min.percent.rule = FALSE, percent = 1)
#' @export

gridUncertainty <- function(input.data, grid.size, n.AOO.improvement,
                            min.percent.rule = FALSE, percent = 1){
  out.df <- data.frame()
  min.grids.shift <- list()
  for (i in 1:n.AOO.improvement){ # First runs before checking for improvement
    out.df[i, 1] <- i
    results <- gridUncertaintyBase(input.data = input.data,
                                   grid.size = grid.size, splits = i,
                                   min.percent.rule = min.percent.rule,
                                   percent = percent)
    out.df[i, 2] <- results$summary.df$min.AOO
    min.grids.shift[[i]] <- results$min.AOO.shifts
  }
  i <- n.AOO.improvement + 1
  logic.test <- FALSE
  while(all(logic.test) == FALSE){
    out.df[i, 1] <- i
    results <- gridUncertaintyBase(input.data = input.data,
                                   grid.size = grid.size, splits = i,
                                   min.percent.rule = min.percent.rule,
                                   percent = percent)
    out.df[i, 2] <- results$summary.df$min.AOO
    min.grids.shift[[i]] <- results$min.AOO.shifts
    logic.test <- vector()
    for (j in 1:(n.AOO.improvement-1)){
      logic.test <- c(logic.test, out.df[(i-n.AOO.improvement), 2] <=
                        out.df[(i-n.AOO.improvement+j), 2])
    }
    i <- i + 1
  }
  names(out.df) <- c('n.splits', 'min.AOO')
  # Find splits which generated the smallest AOOs
  min.AOO.split.index <- which(out.df$min.AOO == min(out.df$min.AOO))
  min.AOO.split.n <- min(min.AOO.split.index) # Only need one of them

  # Get one the shifts needed to get this min AOO
  min.AOO.x.shift <- min.grids.shift[[min.AOO.split.n]]$x.shift[1]
  min.AOO.y.shift <- min.grids.shift[[min.AOO.split.n]]$y.shift[1]

  # Recreate this AOO grid
  original.grid <- createGrid(input.data, grid.size)
  min.AOO.grid.shifts <- shift(original.grid,
                               min.AOO.x.shift,
                               min.AOO.y.shift)
  min.AOO.grid <- getAOOSilent(input.data, min.AOO.grid.shifts,
                               min.percent.rule = min.percent.rule, percent = percent)

  results.list <- list('min.AOO.df' = out.df,
                       'min.AOO.grid' = min.AOO.grid)
  return(results.list)
}

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
#'   the \code{min.percent.rule}.
#' @return Data frame of results showing the minimum and maximum AOO calculated
#'   for each grid shift scenario.
#' @author Calvin Lee \email{calvinkflee@@gmail.com}
#' @family gridUncertainty functions

gridUncertaintySimulation <- function(input.data, grid.size, simulations,
                                      min.percent.rule = FALSE, percent = 1){
  out.df <- data.frame('n.splits' = rep(0, simulations),
                       'min.AOO' = rep(0, simulations),
                       'max.AOO' = rep(0, simulations))
  for(i in 1:simulations){
    out.df[i, 1] <- i
    results <- gridUncertaintyBase(input.data = input.data,
                               grid.size = grid.size, splits = i,
                               min.percent.rule = min.percent.rule, percent = percent)
    out.df[i, 2] <- results$stats$min.AOO
    out.df[i, 3] <- results$stats$max.AOO
  }
  return(out.df)
}

#' Function to compute AOO with grid uncertainty systematically with stopping
#' rule and restrictions
#'
#' \code{gridUncertaintyRestricted} determines the number of area of occupancy (AOO) grid
#' cells occupied by a species or ecosystem systematically. It will only stop
#' when the AOO calculated does not improve (decrease) after a set number of
#' split scenarios. The number of grids within each split is restricted to only
#' include those which are already found nearby to ones already with the minimum
#' AOO.
#' @inheritParams gridUncertainty
#' @return A list containing the following:
#' \itemize{
#'  \item Data frame of results showing the minimum AOO calculated for each
#'  shift scenario
#'  \item Single SpatialPolygonsDataFrame containing the AOO grid which would
#'  produce the minimum AOO calculated
#' }
#' @author Calvin Lee \email{calvinkflee@@gmail.com}
#' @family gridUncertainty functions

gridUncertaintyRestricted <- function(input.data, grid.size, n.AOO.improvement,
                                      min.percent.rule = FALSE, percent = 1){
  out.df <- data.frame()
  min.grids.shift.list <- list()
  n.shifts <- vector()
  for (i in 1:n.AOO.improvement){ # First runs before checking for improvement
    out.df[i, 1] <- i
    results <- gridUncertaintyBase(input.data = input.data,
                                   grid.size = grid.size, splits = i,
                                   min.percent.rule = min.percent.rule,
                                   percent = percent, restriction = FALSE)
    out.df[i, 2] <- results$summary.df$min.AOO
    min.grids.shift.list[[i]] <- results$min.AOO.shifts
    n.shifts <- c(n.shifts, results$summary.df$n.shifts)
    logic.test <- vector()
    for (j in 1:(n.AOO.improvement-1)){
      logic.test <- c(logic.test, out.df[(i-n.AOO.improvement), 2] <=
                        out.df[(i-n.AOO.improvement+j), 2])
    }
  }

  i <- n.AOO.improvement + 1
  logic.test <- FALSE
  while(all(logic.test) == FALSE){
    # Find grids which generated the smallest AOOs
    min.AOO.split.index <- which(out.df[, 2] == min(out.df[, 2]))
    min.grids.shift <- data.frame()
    for(j in 1:length(min.AOO.split.index)){
      min.grids.shift <- rbind(min.grids.shift, min.grids.shift.list[[j]])
    }
    out.df[i, 1] <- i
    results <- gridUncertaintyBase(input.data = input.data,
                                   grid.size = grid.size, splits = i,
                                   min.percent.rule = min.percent.rule,
                                   percent = percent,
                                   restriction = T,
                                   min.grids.shift = min.grids.shift)
    out.df[i, 2] <- results$summary.df$min.AOO
    min.grids.shift.list[[i]] <- results$min.AOO.shifts
    n.shifts <- c(n.shifts, results$summary.df$n.shifts)
    logic.test <- vector()
    for (j in 1:(n.AOO.improvement-1)){
      logic.test <- c(logic.test, out.df[(i-n.AOO.improvement), 2] <=
                        out.df[(i-n.AOO.improvement+j), 2])
    }
    i <- i + 1
  }

  names(out.df) <- c('n.splits', 'min.AOO')
  # Only need to report one grid which resulted in true min.AOO
  min.AOO.split.n <- min(min.AOO.split.index)

  # Get one the shifts needed to get this min AOO
  min.AOO.x.shift <- min.grids.shift.list[[min.AOO.split.n]]$x.shift[1]
  min.AOO.y.shift <- min.grids.shift.list[[min.AOO.split.n]]$y.shift[1]

  # Recreate this AOO grid
  original.grid <- createGrid(input.data, grid.size)
  min.AOO.grid.shifts <- shift(original.grid,
                               min.AOO.x.shift,
                               min.AOO.y.shift)
  min.AOO.grid <- getAOOSilent(input.data, min.AOO.grid.shifts,
                               min.percent.rule = min.percent.rule, percent = percent)

  results.list <- list('min.AOO.df' = out.df,
                       'min.AOO.grid' = min.AOO.grid,
                       'total shifts' = sum(n.shifts))
  return(results.list)
}
