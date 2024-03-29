% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AOO_functions.R
\name{makeAOOGrid}
\alias{makeAOOGrid}
\title{Create Area of Occupancy (AOO) grid for an ecosystem or species distribution}
\usage{
makeAOOGrid(input.data, grid.size, min.percent.rule = FALSE, percent = 1)
}
\arguments{
\item{input.data}{Spatial object of an ecosystem or species distribution.
Please use a CRS with units measured in metres.}

\item{grid.size}{A number specifying the width of the desired grid square (in
same units as your coordinate reference system)}

\item{min.percent.rule}{Logical. If \code{TRUE}, a minimum area threshold
must be passed before a grid is counted as an AOO grid.}

\item{percent}{Numeric. The minimum percent to be applied as a threshold for
the \code{min.percent.rule}}
}
\value{
A shapefile of grid cells occupied by an ecosystem or species
}
\description{
\code{makeAOOGrid} is a generic function that creates grids representing the
area of occupancy for distributions based on the input spatial data. It
includes capability for specifying whether a minimum percent of the grid cell
needs to be occupied before it is counted in the AOO. This functionality is
important for assessing the IUCN Red List of Ecosystems Criteria B.
}
\examples{
crs.UTM55S <- '+proj=utm +zone=55 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs'
r1 <- raster(ifelse((volcano<130), NA, 1), crs = crs.UTM55S)
extent(r1) <- extent(0, 6100, 0, 8700)
AOO_grid <- makeAOOGrid(r1, 1000, min.percent.rule = TRUE, percent = 1)
}
\references{
Bland, L.M., Keith, D.A., Miller, R.M., Murray, N.J. and
Rodriguez, J.P. (eds.) 2016. Guidelines for the application of IUCN Red
List of Ecosystems Categories and Criteria, Version 1.0. Gland,
Switzerland: IUCN. ix + 94pp. Available at the following web site:
\url{https://iucnrle.org/}
}
\seealso{
Other AOO functions: 
\code{\link{createGrid}()},
\code{\link{getAOOSilent}()},
\code{\link{getAOO}()}
}
\author{
Nicholas Murray \email{murr.nick@gmail.com}, Calvin Lee
\email{calvinkflee@gmail.com}
}
\concept{AOO functions}
