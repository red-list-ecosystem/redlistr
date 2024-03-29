% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/change_functions.R
\name{getArea.SpatialPolygons}
\alias{getArea.SpatialPolygons}
\title{Calculates the Area of a Raster from SpatialPolygons.}
\usage{
\method{getArea}{SpatialPolygons}(x, ...)
}
\arguments{
\item{x}{A SpatialPolygons object.}

\item{...}{Addition arguments based on input format}
}
\value{
The total area of the cells of interest in km2
}
\description{
\code{getArea} reports the area of a SpatialPolygons object using sf::st_area
}
\seealso{
Other Change functions: 
\code{\link{getArea.RasterLayer}()},
\code{\link{getArea.SpatRaster}()},
\code{\link{getArea.SpatVect}()},
\code{\link{getArea.sf}()},
\code{\link{getAreaLoss}()},
\code{\link{getArea}()},
\code{\link{getDeclineStats}()}
}
\author{
Nicholas Murray \email{murr.nick@gmail.com}, Calvin Lee
\email{calvinkflee@gmail.com}
}
\concept{Change functions}
