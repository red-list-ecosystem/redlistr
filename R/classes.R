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
  ),
  prototype = list(
    grid = NULL,
    AOO = numeric(0),
    params = list(),
    pctrule = logical(0),
    input = NULL,
    AOOvals = integer(0)
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
        plot(sf::st_geometry(x@grid), border = "grey30",
             col = NA,
             axes = TRUE,                     # adds axes
             xlab = "Easting (m)",            # sensible axis label for projected CRS
             ylab = "Northing (m)",            # sensible axis label for projected CRS
             ...)
        plot(sf::st_geometry(x@input), col = "darkgreen", add = TRUE)
      } else if (inherits(x@input, "SpatRaster")) {
        terra::plot(x@input, col = c("seashell3", "midnightblue"), range = c(0,1))
        # Overlay grid
        plot(sf::st_geometry(x@grid), col = NA, border = "grey30", ..., add = TRUE)
      }
    }



  }
)


#' EOO class
#'
#' A class to represent an EOO convex hull object
#'
#' @slot pol an EOO convex hull polygon
#' @slot EOO the area of the EOO polygon as a numeric
#' @slot unit the unit in which the area is provided
#' @slot input the input ecosystem data used to generate the EOO polygon
#' @export
setClass(
  "EOO",
  slots = list(
    pol =     "sf",
    EOO =     "numeric",
    unit =    "character",
    input =   "geospatial"
  ),
  prototype = list(
    pol = NULL,
    EOO = numeric(0),
    unit = character(0),
    input = NULL
  )
)


#' @export
setMethod("show", "EOO", function(object) {
  cat("Class 'EOO'\n")
  cat("  EOO area:", format(object@EOO, big.mark = ","), object@unit , "\n", sep = " ")
  cat("  Polygon geometry type: ", paste(unique(sf::st_geometry_type(object@pol)), collapse = ", "), "\n", sep = "")
  cat("  Input object class: ", class(object@input)[1], "\n", sep = "")
})

#' @export
setMethod("summary", "EOO", function(object, ...) {
  cat("Summary of EOO object\n")
  cat("----------------------------\n")
  cat("EOO area: ", format(object@EOO, big.mark = ","), " square units\n", sep = "")
  cat("Polygon geometry type(s): ", paste(unique(sf::st_geometry_type(object@pol)), collapse = ", "), "\n", sep = "")
  cat("Number of polygon features: ", nrow(object@pol), "\n", sep = "")
  cat("Input data class: ", class(object@input)[1], "\n", sep = "")

  if (inherits(object@input, "sf")) {
    cat("Input feature count: ", nrow(object@input), "\n", sep = "")
  } else if (inherits(object@input, "SpatRaster")) {
    cat("Input raster layers: ", terra::nlyr(object@input), "\n", sep = "")
    cat("Raster dimensions: ", paste(terra::ext(object@input), collapse = " × "), "\n", sep = "")
  }
  invisible(object)
})

#' @export
setMethod("plot", "EOO", function(x, y, ...) {
  if (is.null(x@pol) || nrow(x@pol) == 0) {
    stop("No polygon data found in slot 'pol'.")
  }
  if (is.null(x@input)) {
    stop("No input data found in slot 'input'.")
  }

  # Set up combined extent if possible
  if (inherits(x@input, "sf")) {
    bbox_all <- sf::st_bbox(sf::st_union(sf::st_geometry(x@pol), sf::st_geometry(x@input)))
  } else if (inherits(x@input, "SpatRaster")) {
    bbox_all <- sf::st_bbox(sf::st_as_sf(terra::as.polygons(x@input)))
  } else {
    stop("Unsupported input type in slot 'input'. Must be 'sf' or 'SpatRaster'.")
  }

  # --- Base plot ---
  plot(sf::st_geometry(x@pol),
       border = "gray12", col = scales::alpha("gray30", 0.2),
       main = "EOO polygon and input data",
       xlim = bbox_all[c("xmin", "xmax")],
       ylim = bbox_all[c("ymin", "ymax")],
       axes = TRUE, ...)

  # --- Overlay input ---
  if (inherits(x@input, "sf")) {
    plot(sf::st_geometry(x@input),
         pch = 21, col = "black", bg = "blue",
         add = TRUE)
  } else if (inherits(x@input, "SpatRaster")) {
    terra::plot(x@input, add = TRUE, col = c("gray45", "midnightblue"))
  }

  box()
})



