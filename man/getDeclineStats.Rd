% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/change_functions.R
\name{getDeclineStats}
\alias{getDeclineStats}
\title{Change statistics.}
\usage{
getDeclineStats(A.t1, A.t2, year.t1, year.t2, methods)
}
\arguments{
\item{A.t1}{Area at time t1}

\item{A.t2}{Area at time t2}

\item{year.t1}{Year of time t1}

\item{year.t2}{Year of time t2}

\item{methods}{Method(s) used to calculate rate of decline. Either 'PRD',
'ARD', and/or 'ARC'. See vignette to see a more detailed explanation for
each of them.}
}
\value{
A dataframe with absolute differences between the two inputs, and a
selection of:
\itemize{
\item Proportional Rate of Decline (PRD)
\item Absolute Rate of Decline (ARD)
\item Annual Rate of Change (ARC)
}
}
\description{
\code{getDeclineStats} calculates the Proportional Rate of Decline (PRD),
Absolute Rate of Decline (ARD) and Annual Rate of Change (ARC), given two
areas at two points in time. Also provides the total area difference. Inputs
are usually the results from \code{getArea}.
}
\examples{
a.r1 <- 23.55
a.r2 <- 15.79
decline.stats <- getDeclineStats(a.r1, a.r2, year.t1 = 1990, year.t2 = 2012,
                                 methods = c('ARD', 'ARC'))
}
\references{
Bland, L.M., Keith, D.A., Miller, R.M., Murray, N.J. and
Rodriguez, J.P. (eds.) 2016. Guidelines for the application of IUCN Red
List of Ecosystems Categories and Criteria, Version 1.0. Gland,
Switzerland: IUCN. ix + 94pp. Available at the following web site:
\url{https://iucnrle.org/}
Puyravaud, J.-P. 2003. Standardizing the calculation of the
annual rate of deforestation. Forest Ecology and Management, 177, 593-596.
}
\seealso{
Other Change functions: 
\code{\link{getArea.RasterLayer}()},
\code{\link{getArea.SpatRaster}()},
\code{\link{getArea.SpatVect}()},
\code{\link{getArea.SpatialPolygons}()},
\code{\link{getArea.sf}()},
\code{\link{getAreaLoss}()},
\code{\link{getArea}()}
}
\author{
Nicholas Murray \email{murr.nick@gmail.com}, Calvin Lee
\email{calvinkflee@gmail.com}
}
\concept{Change functions}
