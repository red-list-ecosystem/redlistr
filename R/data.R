#' Mangrove distribution data
#'
#' Mangrove distribution from the northern regions of Western Port Bay, Victoria, Australia, in the year 2000.
#'
#' @details
#' The dataset is stored as a GeoTIFF file in \code{inst/extdata} and can be loaded with:
#' \preformatted{
#' terra::rast(system.file("extdata", "example_distribution_2000.tif", package = "redlistr"))
#' }
#' @source <https://onlinelibrary.wiley.com/doi/10.1111/j.1466-8238.2010.00584.x/abstract>


#' Mangrove distribution data
#'
#' Mangrove distribution from the northern regions of Western Port Bay, Victoria, Australia, in the year 2017.
#'
#' @details
#' The dataset is stored as a GeoTIFF file in \code{inst/extdata} and can be loaded with:
#' \preformatted{
#' terra::rast(system.file("extdata", "example_distribution_2017.tif", package = "redlistr"))
#' }


#' Mangrove area
#'
#' Area of mangroves in 2000
#' @format ## `a.2000`
#' A data frame with 1 row and 3 columns:
#' \describe{
#'    \item{layer}{name of the layer}
#'    \item{value}{raster value from which area was computed}
#'    \item{area}{area in km2}
#'    }
#'
#'@source computed from mangrove.2000 using getArea
"a.2000"


#' Mangrove area
#'
#' Area of mangroves in 2017
#' @format ## `a.2017`
#' A data frame with 1 row and 3 columns:
#' \describe{
#'    \item{layer}{name of the layer}
#'    \item{value}{raster value from which area was computed}
#'    \item{area}{area in km2}
#'    }
#'
#'@source computed from mangrove.2017 using getArea
"a.2017"

#' Mangrove area combined
#'
#' Area of mangroves in 2000 and 2017
#' @format ## `a`
#' A data frame with 2 rows and 3 columns:
#' \describe{
#'    \item{layer}{name of the layer}
#'    \item{value}{raster value from which area was computed}
#'    \item{area}{area in km2}
#'    }
#'
#'@source computed from mangrove.2000 and mangrove.2017 using getArea
"a"
