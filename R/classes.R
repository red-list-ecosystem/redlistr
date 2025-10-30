#' Union of spatial classes
#'
#' Allows either sf, spatVector, or SpatRaster objects in the spatial slot.
#' @importFrom methods setClassUnion
setClassUnion("geospatial", c("sf", "SpatRaster"))

#' AOOgrid class
#'
#' A class to represent an AOO grid object
#'
#' @slot grid an AOO grid as a shapefile
#' @slot AOO the number of grid squares in grid
#' @slot params a named vector of input parameters (grid size, jitter, n_jitter)
#' @slot pctrule logical indicating whether the 1% rule was applied
#' @slot input the input ecosystem data used to generate the AOO grid
#' @slot AOOvals the list of AOO values
#' @export
setClass(
  "AOOgrid",
  slots = list(
    grid =     "sf",
    AOO =      "numeric",
    params =   "list",
    pctrule =  "logical",
    input =    "geospatial",
    AOOvals =  "integer"
  )
)


#' @export
setMethod(
  "summary", "AOOgrid",
  function(object, ...) {
    cat("Class: AOO_grid\n")
    cat("Number of grid cells:", object@AOO, "\n")

    # Show extent and CRS if available
    if (inherits(object@grid, "sf")) {
      bbox <- sf::st_bbox(object@grid)
      cat("Grid extent:\n")
      print(bbox)
      cat("CRS:", sf::st_crs(object@grid)$input, "\n")
    }

    # Summarise parameters
    cat("\nfunction call parameters:\n")
    print(object@params)

    invisible(object)
  }
)


#' @export
setMethod(
  "show", "AOOgrid",
  function(object) {
    cat("<AOO_grid object>\n")
    cat("  Cells:", object@AOO, "\n")
    if (!is.null(object@params$grid.size))
      cat("  Grid size:", object@params$grid.size, "\n")
    cat("  CRS:", sf::st_crs(object@grid)$epsg, "\n")
    cat("  Geometry type:", unique(sf::st_geometry_type(object@grid)), "\n")
  }
)

#' @export
setMethod(
  "plot", "AOOgrid",
  function(x, y, ...) {
    if (!inherits(x@grid, "sf"))
      stop("Grid must be an sf object to plot.")

    # Base plot
    if (!is.null(x@input)) {
      if (inherits(x@input, "sf")) {
        plot(sf::st_geometry(x@input), col = "darkgreen")
      } else if (inherits(x@input, "SpatRaster")) {
        terra::plot(x@input, col = c("seashell3", "midnightblue"))
      }
    }
    # Overlay grid
    plot(sf::st_geometry(x@grid), col = NA, border = "grey30", ..., add = TRUE)


  }
)
