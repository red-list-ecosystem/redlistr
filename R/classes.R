
#' Union of spatial classes
#'
#' Allows either sf, spatVector, or SpatRaster objects in the spatial slot.
#' @name imports
#' @import methods
#' @import sf
#' @importClassesFrom terra SpatRaster
#' @importFrom rlang .data
#' @keywords internal
NULL

#' AOOgrid class
#'
#' A class to represent an AOO grid object
#'
#' @slot name the name of the assessment unit
#' @slot grid an AOO grid as a shapefile
#' @slot AOO the number of grid squares in grid
#' @slot params a named vector of input parameters (grid size, jitter, n_jitter)
#' @slot pctrule logical indicating whether the 1% rule was applied
#' @slot input the input ecosystem data used to generate the AOO grid
#' @slot AOOvals the list of AOO values
#' @export
methods::setClass(
  "AOOgrid",
  slots = list(
    name =     "character",
    grid =     "sf",
    AOO =      "numeric",
    params =   "list",
    pctrule =  "logical",
    input =    "ANY",
    AOOvals =  "integer"
  ),
  prototype = list(
    name = character(0),
    grid = NULL,
    AOO = numeric(0),
    params = list(),
    pctrule = logical(0),
    input = NULL,
    AOOvals = integer(0)
  )
)


#' AOOgrid summary
#'
#' summary method for AOOgrid object
#'
#' @param object an AOOgrid object
#' @export
methods::setMethod(
  "summary", "AOOgrid",
  function(object) {
    cat(object@name,": AOO_grid\n")
    cat("----------------------------\n")
    cat(" Number of grid cells:", object@AOO, "\n")
    cat(" Area of occupied grids", (object@params$cellsize^2 * object@AOO) / 1000000, "km^2\n")

    # Show extent and CRS if available
    if (inherits(object@grid, "sf")) {
      cat(" CRS:", sf::st_crs(object@grid)$input, "\n")
      bbox <- sf::st_bbox(object@grid)
      cat(" Grid extent:\n")
      print(bbox)
    }

    # Summarise parameters
    cat("\nfunction call parameters:\n")
    cat(" grid size: ", object@params$cellsize, "\n")
    cat(" jitter: ", object@params$jitter, "\n")
    cat(" n_jitter: ", object@params$n, "\n")
    cat("----------------------------\n\n")

    invisible(object)
  }
)

#' AOOgrid show
#'
#' show method for AOOgrid object
#'
#' @param object an AOOgrid object
#' @export
methods::setMethod(
  "show", "AOOgrid",
  function(object) {
    cat("<AOO_grid object>\n")
    cat("  Cells:", object@AOO, "\n")
    if (!is.null(object@params$cellsize))
      cat("  Cell size:", object@params$cellsize, "\n")
    cat("  CRS:", sf::st_crs(object@grid)$epsg, "\n")
  }
)

#' AOOgrid as.list
#'
#' as.list method for AOOgrid object
#'
#' @param x an AOOgrid object
#' @param ... additional arguments
#' @export
#' @method as.list AOOgrid

as.list.AOOgrid <- function(x, ...) {

  # extract CRS safely
  crs_val <- NA_character_
  if (!is.null(x@grid)) {
    crs_obj <- try(sf::st_crs(x@grid), silent = TRUE)
    if (!inherits(crs_obj, "try-error") && !is.null(crs_obj)) {
      if(!is.null(crs_obj$input)) crs_val <- crs_obj$input else crs_val <- as.character(crs_obj$epsg)
    }
  }

  list(
    name        = x@name,
    AOO         = x@AOO,
    area_km2    = (x@params$cellsize^2 * x@AOO) / 1000000,
    crs_grid         = crs_val,
    cellsize    = x@params$cellsize,
    jitter      = x@params$jitter,
    n_jitter    = x@params$n,
    pctrule     = x@pctrule,
    percent     = x@params$pct,
    input_class = paste(class(x@input), collapse = ",")
  )
}


#' AOOgrid plot
#'
#' plot method for AOOgrid object
#'
#' @param x an AOOgrid object
#' @param title plot title, defaults to the assessment unit name.
#' @export
methods::setMethod(
  "plot", "AOOgrid",
  function(x, title = x@name) {
    if (!requireNamespace("leaflet", quietly = TRUE)) {
      stop("Package 'leaflet' is required for plotting. Please install it.",
           call. = FALSE)
    }

    if (!inherits(x@grid, "sf"))
      stop("Grid must be an sf object to plot.")

    # Leaflet plot
    if (!is.null(x@input)) {
      p <- leaflet::leaflet() |>
        leaflet::addProviderTiles("CartoDB.Positron") |>
        leaflet::addPolygons(data = sf::st_transform(x@grid, 4326), color = "black", weight = 1, fillColor = "none", group = "AOOgrid") |>
         leaflet::addControl(
          html = paste("<div style='background:white; padding:8px; font-weight:bold;'>
              ", title, "</div>"),
          position = "topright") |>
        leaflet::addLayersControl(overlayGroups = c("Input", "AOOgrid"), options = leaflet::layersControlOptions(collapsed = FALSE)) |>
        leaflet::addControl(
          html = paste("<div style='background:white; padding:8px;'>
              <b>AOO: </b><br/>
              ", x@AOO, "cells
            </div>"),
          position = "topright")



      if (inherits(x@input, "sf")) {
        if(any(st_geometry_type(x@input) |> unique() |> as.character() %in% c("POLYGON", "MULTIPOLYGON"))){
          p <- p |>
            leaflet::addPolygons(data = sf::st_transform(x@input, 4326), fillColor = "darkgreen", fillOpacity = 0.85, color = "gray30", weight = 1, group = "Input")

        }
        if(any(st_geometry_type(x@input) |> unique() |> as.character() %in% c("POINT", "MULTIPOINT"))){
          p <- p |>
            leaflet::addCircleMarkers(data = sf::st_transform(x@input, 4326), radius = 3, color = "darkgreen", weight = 0, group = "Input")
        }

      } else if (inherits(x@input, "SpatRaster")) {
        p <- p |>
          leaflet::addRasterImage(x = terra::project(x@input, "EPSG:4326"),
                                  colors = function(x) ifelse(is.na(x), NA, "darkgreen"),
                                  opacity = 0.85,
                                  group = "Input")

      }
    }
   p
  }
)


#' AOOgrid hist
#'
#' hist method for AOOgrid objects
#'
#' @param x an AOOgrid object
#' @param ... Additional graphical parameters passed to \code{\link[graphics]{hist}}.
#' @export

methods::setMethod(
  "hist", "AOOgrid",
  function(x, ...){
    if (!inherits(x@AOOvals, "integer"))
      stop("AOOvals must be an integer vector to plot histograms.")
    if(x@params$n == 1)
      stop("Grid was not jittered")
    if(x@params$n > 1)
      if(diff(range(x@AOOvals)) <= 30){by <- 1}else{by <- 2}
      breaks <- seq(from = min(x@AOOvals-.5), to = max(x@AOOvals+.5), by = by)
      hist(x@AOOvals, main = paste(x@name,"\n Distribution of cell counts under", x@params$n, "iterations"),
           xlab = "AOO cell count", ylab = "Frequency",
           breaks = breaks, col = "dodgerblue4", las = 1, ...)
      box()
  })


#' EOO class
#'
#' A class to represent an EOO convex hull object
#'
#' @slot name the name of the assessment unit
#' @slot pol an EOO convex hull polygon
#' @slot EOO the area of the EOO polygon as a numeric
#' @slot unit the unit in which the area is provided
#' @slot input the input ecosystem data used to generate the EOO polygon
#' @export
methods::setClass(
  "EOO",
  slots = list(
    name =    "character",
    pol =     "sf",
    EOO =     "numeric",
    unit =    "character",
    input =   "ANY"
  ),
  prototype = list(
    name = character(0),
    pol = NULL,
    EOO = numeric(0),
    unit = character(0),
    input = NULL
  )
)


#' EOO show
#'
#' show method for EOO object
#'
#' @param object an EOO object
#' @export
methods::setMethod("show", "EOO", function(object) {
  cat("Class 'EOO'\n")
  cat("  EOO area:", format(object@EOO, big.mark = ","), object@unit , "\n", sep = " ")
  cat("  CRS:", sf::st_crs(object@pol)$input, "\n")
  cat("  Input object class: ", class(object@input)[1], "\n", sep = "")
})

#' EOO summary
#'
#' summary method for EOO object
#'
#' @param object an EOO object
#' @export
methods::setMethod("summary", "EOO", function(object) {
  cat(object@name, ": EOO object\n")
  cat("----------------------------\n")
  cat(" EOO area: ", format(object@EOO, big.mark = ","), " square kms\n", sep = "")
  cat(" CRS:", sf::st_crs(object@pol)$input, "\n")
  cat(" Input data class: ", class(object@input)[1], "\n", sep = "")

  if (inherits(object@input, "sf")) {
    cat(" Input feature count: ", nrow(object@input), "\n", sep = "")
    cat("----------------------------\n")
  } else if (inherits(object@input, "SpatRaster")) {
    cat(" Input raster layers: ", terra::nlyr(object@input), "\n", sep = "")
    cat(" Raster dimensions: ", paste(terra::ext(object@input), collapse = " x "), "\n", sep = "")
    cat("----------------------------\n")
  }
  invisible(object)
})

#' EOO as.list
#'
#' as.list method for EOO object
#'
#' @param x an EOO object
#' @param ... additional arguments
#' @export
#' @method as.list EOO

as.list.EOO <- function(x, ...) {

  # extract CRS safely
  crs_val <- NA_character_
  if (!is.null(x@pol)) {
    crs_obj <- try(sf::st_crs(x@pol), silent = TRUE)
    if (!inherits(crs_obj, "try-error") && !is.null(crs_obj)) {
      if(!is.null(crs_obj$input)) crs_val <- crs_obj$input else crs_val <- as.character(crs_obj$epsg)
    }
  }

  list(
    name        = x@name,
    EOO         = x@EOO,
    unit        = x@unit,
    crs_polygon = crs_val,
    input_class = paste(class(x@input), collapse = ",")
  )
}

#' EOO plot
#'
#' plot method for EOO object
#'
#' @param x an EOO object
#' @param title Plot title, defaults to name of assessment unit.
#' @importFrom graphics box
#' @export
methods::setMethod("plot", "EOO", function(x, title = x@name) {
  if (!requireNamespace("leaflet", quietly = TRUE)) {
    stop("Package 'leaflet' is required for plotting. Please install it.",
         call. = FALSE)
  }

  if (is.null(x@pol) || nrow(x@pol) == 0) {
    stop("No polygon data found in slot 'pol'.")
  }
  if (is.null(x@input)) {
    stop("No input data found in slot 'input'.")
  }

  # --- Leaflet plot ---
  p <-  leaflet::leaflet() |>
    leaflet::addProviderTiles("CartoDB.Positron") |>
    leaflet::addPolygons(data = sf::st_transform(x@pol, 4326),
                         color = "black",
                         weight = 1, fillColor = "gray", fillOpacity = 0.2,
                         group = "EOO_polygon") |>
    leaflet::addControl(
      html = paste("<div style='background:white; padding:8px; font-weight:bold;'>
              ", title, "</div>"),
      position = "topright") |>
    leaflet::addLayersControl(overlayGroups = c("Input", "EOO_polygon"),
                              options = leaflet::layersControlOptions(collapsed = FALSE)) |>
    leaflet::addControl(
      html = paste("<div style='background:white; padding:8px;'>
              <b>EOO area: </b><br/>
              ", round(x@EOO, 2), "km^2
            </div>"),
      position = "topright")


  if (inherits(x@input, "sf")) {
    if(any(st_geometry_type(x@input) |> unique() |> as.character() %in% c("POLYGON", "MULTIPOLYGON"))){
      p <- p |> leaflet::addPolygons(data = sf::st_transform(x@input, 4326), fillColor = "darkgreen", fillOpacity = 0.85, color = "none", weight = 0, group = "Input")
    }
    if(any(st_geometry_type(x@input) |> unique() |> as.character() %in% c("POINT", "MULTIPOINT"))){
      p <- p |> leaflet::addCircleMarkers(data = sf::st_transform(x@input, 4326), radius = 3, color = "darkgreen", weight = 0, group = "Input")
      }

  } else if (inherits(x@input, "SpatRaster")) {
    p <- p |> leaflet::addRasterImage(x = terra::project(x@input, "EPSG:4326"),
                                      colors = function(x) ifelse(is.na(x), NA, "darkgreen"),
                                      opacity = 0.85,
                                      group = "Input")
  }

  p
})


#' trend class
#'
#' A class to represent change in area over time for an ecosystem.
#'
#' @slot input binary raster or list of sf POLYGON objects representing the input ecosystem extent
#' @slot areas the area of the ecosystem in each layer or list element
#' @slot model a fitted model object typically of the class lm containing the model used to calculate the trend
#' @slot netdiff difference between the area in the first layer or list element and the last.
#' @slot diff raster stack showing change over layers or list elements of the ecosystem extent.
#' @export
methods::setClass(
  "trend",
  slots = list(
    input =    "ANY",
    areas =    "data.frame",
    model =    "lm",
    netdiff =  "numeric",
    diff =     "ANY"
  ),
  prototype = list(
    input = NULL,
    areas = data.frame(),
    model = NULL,
    netdiff = numeric(0),
    diff = NULL
  )
)


#' trend summary
#'
#' summary method for trend object
#'
#' @param object a trend object
#' @export
methods::setMethod("summary", "trend",
                   function(object) {
  #predictions
  newdat <- data.frame(t = seq(min(object@areas$t), max(object@areas$t), length.out = 100))
  pred <- predict(object@model, newdata = newdat, type = "link", se.fit = TRUE)
  pred_df <- data.frame(
    t       = newdat$t,
    mean     = exp(pred$fit),
    l95 = exp(pred$fit - 1.96 * pred$se.fit),
    u95 = exp(pred$fit + 1.96 * pred$se.fit))

  pred_change <- pred_df$mean[nrow(pred_df)]-pred_df$mean[1]

  #printout
  cat("Summary of ecosystem trend\n")
  cat("----------------------------\n")
  cat("Net change in area:\n", format(object@netdiff, big.mark = ","), "kms squared\n\n")
  cat("Modeled change in area:\n", format(pred_change, big.mark = ","), "kms squared\n\n")
  cat("Modeled percent change in area:\n", format(100*pred_change/pred_df$mean[1], big.mark = ","), "%\n\n")
  cat("Ecosystem area:\n", object@areas$area, "\n")
  cat("Input data class: ", class(object@input)[1], "\n", sep = "")

  if (inherits(object@input, "sf")) {
    cat("Input feature count: ", nrow(object@input), "\n", sep = "")
  } else if (inherits(object@input, "SpatRaster")) {
    cat("Input raster layers: ", terra::nlyr(object@input), "\n", sep = "")
    cat("Raster dimensions: ", paste(terra::ext(object@input), collapse = " x "), "\n", sep = "")
  }
  invisible(object)

  #plotting_code

  ggplot2::ggplot() +
    # original data
    ggplot2::geom_point(data = object@areas, ggplot2::aes(x = t, y = area), size = 2) +
    # CI ribbon
    ggplot2::geom_ribbon(data = pred_df,
                         ggplot2::aes(x = t, ymin = .data$l95, ymax = .data$u95),
                         alpha = 0.25) +

    # fitted mean line
    ggplot2::geom_line(data = pred_df,
                       ggplot2::aes(x = t, y = mean),
                       linewidth = 1, col = "dodgerblue4") +

    ggplot2::labs(
      x = "Time",
      y = "Value",
      title = "Observed Data and Fitted Log-Link Model with 95% CI"
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

})


#' trend plot
#'
#' plot method for trend object
#'
#' @param x an trend object
#' @param y Ignored. Included for consistency with the \code{plot} generic.
#' @param ... Additional graphical parameters passed to \code{\link[graphics]{plot}}.
#' @export
methods::setMethod("plot", "trend",
                   function(x, ...) {
  if (is.null(x@input) || nrow(x@input) == 0) {
    stop("No data found in slot 'input'.")
  }
  if (is.null(x@diff)) {
    stop("No data found in slot 'diff'.")
  }

  # --- Compute combined extent (same as before) ---
  if (inherits(x@input, "sf")) {
    bbox_all <- sf::st_bbox(sf::st_union(sf::st_geometry(x@input)))
  } else if (inherits(x@input, "SpatRaster")) {
    bbox_all <- sf::st_bbox(sf::st_as_sf(terra::as.polygons(x@input)))
  } else {
    stop("Unsupported input type. Must be 'sf' or 'SpatRaster'.")
  }

  # --- Convert SpatRaster to data frame for ggplot ---
  # keep only the first layer of diff if multi-layer
  r <- diff[[1]]

  # terra::as.data.frame with xy = TRUE for coordinates
  r_df <- terra::as.data.frame(r, xy = TRUE, na.rm = FALSE)
  names(r_df)[3] <- "value"   # rename the raster column

  # Make value a factor with labels for 0,1,2,3
  r_df$value <- factor(
    r_df$value,
    levels = c(0, 1, 2, 3),
    labels = c("NA", "Lost", "Gained", "Kept")
  )

  # --- Build ggplot ---
  p <- ggplot2::ggplot(r_df) +
    ggplot2::geom_raster(
      ggplot2::aes(x = x, y = y, fill = .data$value)
    ) +
    ggplot2::coord_sf(
      xlim = c(bbox_all["xmin"], bbox_all["xmax"]),
      ylim = c(bbox_all["ymin"], bbox_all["ymax"]),
      expand = FALSE
    ) +
    ggplot2::scale_fill_viridis_d(
      name = "Ecosystem change"  # legend title
    ) +
    ggplot2::labs(
      title = "Ecosystem change"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      # legend outside the main plot area
      legend.position = "right",
      legend.box = "vertical",
      legend.title = ggplot2::element_text(size = 10),
      legend.text  = ggplot2::element_text(size = 9),
      plot.title   = ggplot2::element_text(hjust = 0.5)
    )

  print(p)
})
