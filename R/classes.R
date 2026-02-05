
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
    grid =     "sf",
    AOO =      "numeric",
    params =   "list",
    pctrule =  "logical",
    input =    "ANY",
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


#' AOOgrid summary
#'
#' summary method for AOOgrid object
#'
#' @param object an AOOgrid object
#' @export
methods::setMethod(
  "summary", "AOOgrid",
  function(object) {
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
    cat("grid size: ", object@params$gridsize, "\n")
    cat("jitter: ", object@params$jitter, "\n")
    cat("n_jitter: ", object@params$n, "\n\n\n")

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
    if (!is.null(object@params$grid.size))
      cat("  Grid size:", object@params$grid.size, "\n")
    cat("  CRS:", sf::st_crs(object@grid)$epsg, "\n")
    cat("  Geometry type:", unique(sf::st_geometry_type(object@grid)), "\n")
  }
)

#' AOOgrid plot
#'
#' plot method for AOOgrid object
#'
#' @param x an AOOgrid object
#' @param y Ignored. Included for consistency with the \code{plot} generic.
#' @param ... Additional graphical parameters passed to \code{\link[graphics]{plot}}.
#' @export
methods::setMethod(
  "plot", "AOOgrid",
  function(x, ...) {
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
        plot(sf::st_geometry(x@input), col = "darkgreen", add = TRUE, ...)
      } else if (inherits(x@input, "SpatRaster")) {
        # TODO FIX THIS
        unionextent <- terra::union(terra::ext(x@input), sf::st_bbox(x@grid) |> terra::ext())
        x@input <- terra::extend(x@input, unionextent)
        x@input <- terra::extend(x@input, 10)
        #x@input <- terra::extend(x@input, x@params$gridsize/60)
        terra::plot(x@input, col = c("seashell3", "midnightblue"), range = c(0,1), ...)
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
methods::setClass(
  "EOO",
  slots = list(
    pol =     "sf",
    EOO =     "numeric",
    unit =    "character",
    input =   "ANY"
  ),
  prototype = list(
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
  cat("  Polygon geometry type: ", paste(unique(sf::st_geometry_type(object@pol)), collapse = ", "), "\n", sep = "")
  cat("  Input object class: ", class(object@input)[1], "\n", sep = "")
})

#' EOO summary
#'
#' summary method for EOO object
#'
#' @param object an EOO object
#' @export
methods::setMethod("summary", "EOO", function(object) {
  cat("Summary of EOO object\n")
  cat("----------------------------\n")
  cat("EOO area: ", format(object@EOO, big.mark = ","), " square kms\n", sep = "")
  cat("Polygon geometry type(s): ", paste(unique(sf::st_geometry_type(object@pol)), collapse = ", "), "\n", sep = "")
  cat("Number of polygon features: ", nrow(object@pol), "\n", sep = "")
  cat("Input data class: ", class(object@input)[1], "\n", sep = "")

  if (inherits(object@input, "sf")) {
    cat("Input feature count: ", nrow(object@input), "\n\n", sep = "")
  } else if (inherits(object@input, "SpatRaster")) {
    cat("Input raster layers: ", terra::nlyr(object@input), "\n", sep = "")
    cat("Raster dimensions: ", paste(terra::ext(object@input), collapse = " x "), "\n\n", sep = "")
  }
  invisible(object)
})

#' EOO plot
#'
#' plot method for EOO object
#'
#' @param x an EOO object
#' @param y Ignored. Included for consistency with the \code{plot} generic.
#' @param ... Additional graphical parameters passed to \code{\link[graphics]{plot}}.
#' @importFrom graphics box
#' @export
methods::setMethod("plot", "EOO", function(x, ...) {
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
    terra::plot(x@input, add = TRUE, col = c("gray55", "midnightblue"))
    plot(sf::st_geometry(x@pol), add = TRUE)
  }

  graphics::box()
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
