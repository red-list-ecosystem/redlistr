#' Compute Area of Occupancy (AOO) accounting for grid uncertainty
#'
#' \code{gridUncertainty} determines the number of area of occupancy (AOO) grid
#' cells occupied by a species or ecosystem. The key difference with
#' \code{getAOO} is that it accounts for the impact of the location of the AOO
#' grid by randomly moving the grid and recomputing AOO at each step.
#'
#' @param x Raster object of an ecosystem or species distribution
#' @param grid Raster object of a regular grid suitable for assessing AOO
#' @param nSim specifies the number of times to repeat the AOO calculation
#' with randomly sampled grid origin coordinates
#' @return A list of dataframes raw.sims and summary.sims
#' @author Nicholas Murray \email{murr.nick@@gmail.com}
#' @seealso \code{\link{createGrid}} \code{\link{getAOO}}
#' @references Bland, L.M., Keith, D.A., Miller, R.M., Murray, N.J. and
#' Rodriguez, J.P. (eds.) 2016. Guidelines for the application of IUCN Red
#' List of Ecosystems Categories and Criteria, Version 1.0. Gland, Switzerland:
#' IUCN. ix + 94pp. Available at the following web site: \url{iucnrle.org/}
#' @examples
# TODO: do the examples

gridUncertainty <- function (ecosystem.data, grid.size, nSim = 10, one.percent.rule = FALSE){

  grid <- createGrid(ecosystem.data, grid.size)
  results.df <- data.frame(sim.no = integer(),
                           x.shift = integer(),
                           y.shift = integer(),
                           dist.move = double(),
                           AOO = integer(),
                           stringsAsFactors = FALSE)


  for (i in 1:nSim){

    message (paste("Starting grid uncertainty number: ",i, "of", nSim))
    x.shift = sample(-10000:10000, 1)
    message (paste("... moved x by:", x.shift))
    y.shift = sample(-10000:10000, 1)
    message (paste("... moved y by:", y.shift))
    dist.move = sqrt((x.shift^2)+(y.shift^2))# use pythagoras to work out how far it was moved as a function of a right-angled triangle

    grid.shift <- shift(grid, x=x.shift, y=y.shift) # move the grid
    AOO = getAOOSilent (ecosystem.data, grid = grid.shift, one.percent.rule = one.percent.rule) # get the AOO again


    message (paste("... AOO equals:", AOO))
    sim.df <-  data.frame(sim.no = i,
                          x.shift = x.shift,
                          y.shift = y.shift,
                          dist.move = dist.move,
                          AOO = AOO,
                          stringsAsFactors = FALSE)

    results.df <- rbind(results.df, sim.df)
  }
  min.AOO <-  min (results.df$AOO)
  message (paste("Minimum AOO is from", nSim, "assessments is:", min.AOO, "grid cells"))
  out.df <- data.frame (n.sims = nSim,
                        mean.AOO = mean(results.df$AOO),
                        var.AOO = var(results.df$AOO),
                        sd.AOO = sd (results.df$AOO),
                        min.AOO = min (results.df$AOO),
                        max.AOO = max (results.df$AOO))

  hist.breaks = seq(min(results.df$AOO),max(results.df$AOO),1)
  hist(results.df$AOO, breaks = hist.breaks, col = "darkred", main = "Histogram: AOO Uncertainty")
  return (list(raw.sims = results.df, summary.sims = out.df))
}

# TODO: change the 1% rule to a number between 0 and 1 specifying how much of the grid cell must be occupied before being selected for AOO
# TODO: can we get a raster or vector output of the final AOO? Should be possible by passing the grid cell IDs here and selecting the grid cells from the grid object
n = gridUncertainty(r1, grid.size = 10000, nSim = 100, one.percent.rule = FALSE)
