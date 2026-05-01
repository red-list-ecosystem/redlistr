#' Safely transform sf object CRS
#' @keywords internal
#'
safe_transform <- function(x, crs) {
  tryCatch(
    sf::st_transform(x, crs),
    error = function(e) x
  )
}

#' Safely project SpatRaster object CRS
#' @keywords internal
safe_project <- function(x, crs) {
  tryCatch(
    terra::project(x, crs),
    error = function(e) x
  )
}
