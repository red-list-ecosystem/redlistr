#' Calculates the Area of ecosystems in spatial data
#'
#' `getArea` reports the area of ecosystem units provided as spatial data
#' @param x A SpatRaster, SpatVector, or an sf object with POLYGONS geometry.
#' @param names_from  a column names containing ecosystem labels, as a string or
#' dplyr-style column name. Only required for SpatVector and sf types. Units are
#' assumed to be delineated by raster value for SpatRasters.
#' @param ... Addition arguments based on input format
#' @return A data frame containing ecosystem identifiers and the total area of the
#' ecosystem units in x as a units vector (km^2). For raster bricks it also contains
#' the layer number.
#' @author Nicholas Murray \email{murr.nick@@gmail.com}, Calvin Lee
#'   \email{calvinkflee@@gmail.com}, Aniko B. Toth \email{anikobtoth@@gmail.com}
#' @family Change functions
#' @examples
#' crs.UTM55S <- '+proj=utm +zone=55 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs'
#' r1 <- raster(ifelse((volcano<130), NA, 1), crs = crs.UTM55S)
#' extent(r1) <- extent(0, 6100, 0, 8700)
#' a.r1 <- getArea(r1) # area of all non-NA cells in r1
#' @export
#' @import terra
#' @import sf
#' @import dplyr
#' @import rlang
#' @import units

getArea <- function(x, names_from = NA, ...){
  if(st_is_longlat(x)){
    stop('Input has a longitude/latitude CRS.\nPlease reproject to a projected coordinate system')
  }
  UseMethod("getArea", x)
}

#' @export
getArea.SpatRaster <- function(x, ...){
  area <- terra::expanse(x, "km", byValue=TRUE, usenames = TRUE)
  return(area)
}

#' @export
getArea.SpatVector <- function(x, names_from = NA, ...){
  x <- st_as_sf(x)
  if(missing(names_from)){
    return(
      x |>
        dplyr::summarise(geometry = sf::st_union(geometry))|>
        dplyr::mutate(area = sf::st_area(geometry) |> units::set_units(km^2)) |>
        sf::st_drop_geometry() |>
        as.data.frame() |>
        dplyr::mutate(ecosystem_name = "unnamed ecosystem") |>
        dplyr::mutate(area_km2 = as.numeric(area)) |>
        dplyr::select(-area)
    )
  }

  # Turn input into a symbol (works for bare names AND strings)
  var <- ensym(names_from)

  # group by ecosystem names and get areas.
  x |>
    group_by(.data[[rlang::as_string(var)]]) |>
    summarise(geometry = st_union(geometry))|>
    mutate(area = st_area(geometry) |> set_units(km^2)) |>
    st_drop_geometry() |>
    as.data.frame() |> mutate(area_km2 = as.numeric(area)) |>
    select(-area)
}

#' @export
getArea.sf <- function(x, names_from = NA, ...) {
  if(missing(names_from)){
    return(
      x |>
      dplyr::summarise(geometry = sf::st_union(geometry))|>
      dplyr::mutate(area = sf::st_area(geometry) |> units::set_units(km^2)) |>
      sf::st_drop_geometry() |>
      as.data.frame() |>
      dplyr::mutate(ecosystem_name = "unnamed ecosystem") |>
      dplyr::mutate(area_km2 = as.numeric(area)) |>
      dplyr::select(-area)
    )
  }

  # Turn input into a symbol (works for bare names AND strings)
  var <- ensym(names_from)
  # group by ecosystem names and get areas.
  x |>
    group_by(.data[[rlang::as_string(var)]]) |>
    summarise(geometry = st_union(geometry))|>
    mutate(area = st_area(geometry) |> set_units(km^2)) |>
    st_drop_geometry() |>
    as.data.frame() |> mutate(area_km2 = as.numeric(area)) |>
    select(-area)
}


#' Area change between two inputs in km2
#'
#' `getAreaChange` reports the difference in area between two inputs. Inputs
#' can be  SpatRaster, SpatVector, sf or a data frame of areas and may contain
#' data for multiple ecosystem types. Ensure x and y are the same data type.
#' If using data frame as input, ensure areas are measured in km2
#'
#' @param x SpatRaster, SpatVector, or sf object representing one or more
#' ecosystems or a data frame with two columns, one of them labeled "area",
#' and the other containing ecosystem labels names_from_x.
#' @param y SpatRaster, SpatVector, or sf object representing one or more
#' ecosystems or a data frame with two columns, one of them labeled "area",
#' and the other containing ecosystem labels names_from_y.
#' @param names_from_x name of column containing ecosystem labels. Ignored if
#' x is a raster.
#' @param names_from_y name of column containing ecosystem labels. Ignored if
#' y is a raster. names_from_x used if not provided.
#' @return Returns a table containing ecosystem labels, areas, and the
#' difference in area of the two inputs in km2
#' @author Nicholas Murray \email{murr.nick@@gmail.com}, Calvin Lee
#'   \email{calvinkflee@@gmail.com}, Aniko B. Toth \email{anikobtoth@@gmail.com}
#' @family Change functions
#' @examples
#' crs.UTM55S <- '+proj=utm +zone=55 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs'
#' r1 <- raster(ifelse((volcano<130), NA, 1), crs = crs.UTM55S)
#' extent(r1) <- extent(0, 6100, 0, 8700)
#' r2 <- raster(ifelse((volcano<145), NA, 1), crs = crs.UTM55S)
#' extent(r2) <- extent(0, 6100, 0, 8700)
#' a.dif <- getAreaChange(r1, r2) # distribution rasters
#'
#' @export
#' @import dplyr
#' @import rlang

getAreaChange <- function(x, y, names_from_x = NA, names_from_y = NA){
 UseMethod("getAreaChange", x)
}

#' @export
getAreaChange.SpatRaster <- function(x, y){
  a.x <- getArea(x)
  a.y <- getArea(y)
  adiff <- dplyr::full_join(a.x, a.y, by = "value") |>
    dplyr::mutate(area_change = area.y - area.x) |>
    dplyr::select(-layer.x, -layer.y)

  return(adiff)
}

#' @export
getAreaChange.SpatVector <- function(x, y, names_from_x = NA, names_from_y = NA){

  if(missing(names_from_x)){
    if(missing(names_from_y)){
      a.x <- getArea(x)
      a.y <- getArea(y)
      return(
        dplyr::full_join(a.x, a.y, by = "ecosystem_name") |>
        dplyr::mutate(area_diff = area_km2.y - area_km2.x)
      )
    }else{
      nfy <- ensym(names_from_y)
      a.x <- getArea(x, names_from = {{names_from_y}})
      a.y <- getArea(y, names_from = {{names_from_y}})
      return(
        dplyr::full_join(a.x, a.y, by = as_string(nfy)) |>
        dplyr::mutate(area_diff = area_km2.y - area_km2.x)
      )
    }
  }else if(missing(names_from_y)){
    message("names_from_y not provided, using names_from_x for both datasets")
    nfx <- ensym(names_from_x)
    a.x <- getArea(x, names_from = {{names_from_x}})
    a.y <- getArea(y, names_from = {{names_from_x}})
    return(
      dplyr::full_join(a.x, a.y, by = as_string(nfx)) |>
      dplyr::mutate(area_diff = area_km2.y - area_km2.x)
      )
  }else{

  nfx <- ensym(names_from_x)
  nfy <- ensym(names_from_y)

  a.x <- getArea(x, names_from = {{names_from_x}})
  a.y <- getArea(y, names_from = {{names_from_y}})
  return(
    dplyr::full_join(a.x, a.y, by = as_string(nfy) |> stats::setNames(as_string(nfx))) |>
    dplyr::mutate(area_diff = area_km2.y - area_km2.x)
  )
  }

}

#' @export
getAreaChange.sf <- function(x, y, names_from_x = NA, names_from_y = NA) {

  if(missing(names_from_x)){
    if(missing(names_from_y)){
      a.x <- getArea(x)
      a.y <- getArea(y)
      return(
        dplyr::full_join(a.x, a.y, by = "ecosystem_name") |>
          dplyr::mutate(area_diff = area_km2.y - area_km2.x)
      )
    }else{
      nfy <- ensym(names_from_y)
      a.x <- x |> getArea(names_from = {{names_from_y}})
      a.y <- y |> getArea(names_from = {{names_from_y}})
      return(
        dplyr::full_join(a.x, a.y, by = as_string(nfy)) |>
          dplyr::mutate(area_diff = area_km2.y - area_km2.x)
      )
    }
  }else if(missing(names_from_y)){
    message("names_from_y not provided, using names_from_x for both datasets")
    nfx <- ensym(names_from_x)
    a.x <- x |> getArea(names_from = {{names_from_x}})
    a.y <- y |> getArea(names_from = {{names_from_x}})
    return(
      dplyr::full_join(a.x, a.y, by = as_string(nfx)) |>
        dplyr::mutate(area_diff = area_km2.y - area_km2.x)
    )
  }else{

    nfx <- ensym(names_from_x)
    nfy <- ensym(names_from_y)

    a.x <- x |> getArea(names_from = {{names_from_x}})
    a.y <- y |> getArea(names_from = {{names_from_y}})
    return(
      dplyr::full_join(a.x, a.y, by = as_string(nfy) |> stats::setNames(as_string(nfx))) |>
        dplyr::mutate(area_diff = area_km2.y - area_km2.x)
    )
  }

}

#' @export
getAreaChange.data.frame <- function(x, y, names_from_x = NA, names_from_y = NA) {
  a.x <- x
  a.y <- y

  if(missing(names_from_x)){
    if(missing(names_from_y)){
      stop("At least one names_from column must be given")
    }else{
      nfy <- ensym(names_from_y)
      return(dplyr::full_join(a.x, a.y, by = as_string(nfy)) |>
               dplyr::mutate(area_diff = area.y - area.x))
    }
  }else if(missing(names_from_y)){
    nfx <- ensym(names_from_x)
    return(dplyr::full_join(a.x, a.y, by = as_string(nfx)) |>
             dplyr::mutate(area_diff = area.y - area.x))
  }else{

    nfx <- ensym(names_from_x)
    nfy <- ensym(names_from_y)

    return(dplyr::full_join(a.x, a.y, by = as_string(nfy) |> stats::setNames(as_string(nfx))) |>
             dplyr::mutate(area_diff = area.y - area.x))
  }

}

#' Calculate area trends of ecosystems
#'
#' `getAreaTrend` is used to calculate changes in area over time. Output
#' is a list can be used to easily visualise trends for one ecosystem at a time.
#' @param x SpatRaster with multiple layers or a list of sf or SpatVector objects.
#' @param names_from name of column containing ecosystem labels. Ignored if
#' x is a raster.

#' @return returns a list
#' @author Aniko B. Toth \email{anikobtoth@@gmail.com}
#' @family Change functions
#' @examples
#' crs.UTM55S <- '+proj=utm +zone=55 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs'
#' r1 <- raster(ifelse((volcano<130), NA, 1), crs = crs.UTM55S)
#' extent(r1) <- extent(0, 6100, 0, 8700)
#' r2 <- raster(ifelse((volcano<145), NA, 1), crs = crs.UTM55S)
#' extent(r2) <- extent(0, 6100, 0, 8700)
#' a.dif <- getAreaChange(r1, r2) # distribution rasters
#' @export
#' @import dplyr
#' @import terra

getAreaTrend <- function(x, names_from = NA){
  UseMethod("getAreaTrend", x)
}

#' @export
getAreaTrend.SpatRaster <- function(x){
 vals <- terra::values(x) |> as.vector() |> unique() |> sort()
  binary_stacks <- lapply(vals, function(v) {
                             binaries <- as.numeric(x == v)
    return(binaries)
  })
  names(binary_stacks) <- paste0("value_", vals)

  areas <- lapply(binary_stacks, getArea) |> lapply(function(v) v |> dplyr::filter(value == 1) |> dplyr::select(-value))

  trend_list <- lapply(1:length(binary_stacks),
                         function(i) list(
                                         input = binary_stacks[[i]],
                                         areas = areas[[i]],
                                         netdiff = last(areas[[i]]$area) - first(areas[[i]]$area),
                                         diff = binary_stacks[[i]][[nlyr(x)]]*2+binary_stacks[[i]][[1]])  # 1 = lost, 2 = gained, 3 = kept
                                         ) |>
    setNames(paste0("value_",vals))


return(trend_list)
}


# TODO; getAreaTrend for list of polygon layers.

#' Change statistics.
#'
#' `getDeclineStats` calculates the Proportional Rate of Decline (PRD),
#' Absolute Rate of Decline (ARD) and Annual Rate of Change (ARC), given two
#' areas at two points in time. Also provides the total area difference. Inputs
#' are usually the results from `getArea`.
#'
#' @param A.t1 Area at time t1
#' @param year.t1 Year of time t1
#' @param A.t2 Area at time t2
#' @param year.t2 Year of time t2
#' @param methods Method(s) used to calculate rate of decline. Either 'PRD',
#'   'ARD', and/or 'ARC'. See vignette to see a more detailed explanation for
#'   each of them.
#' @return A dataframe with absolute differences between the two inputs, and a
#'   selection of:
#' \itemize{
#'  \item Proportional Rate of Decline (PRD)
#'  \item Absolute Rate of Decline (ARD)
#'  \item Annual Rate of Change (ARC)
#'  }
#' @author Nicholas Murray \email{murr.nick@@gmail.com}, Calvin Lee
#'   \email{calvinkflee@@gmail.com}
#' @family Change functions
#' @references Bland, L.M., Keith, D.A., Miller, R.M., Murray, N.J. and
#'   Rodriguez, J.P. (eds.) 2016. Guidelines for the application of IUCN Red
#'   List of Ecosystems Categories and Criteria, Version 1.0. Gland,
#'   Switzerland: IUCN. ix + 94pp. Available at the following web site:
#'   <https://iucnrle.org/>
#'   Puyravaud, J.-P. 2003. Standardizing the calculation of the
#'   annual rate of deforestation. Forest Ecology and Management, 177, 593-596.
#' @examples
#' a.r1 <- 23.55
#' a.r2 <- 15.79
#' decline.stats <- getDeclineStats(a.r1, a.r2, year.t1 = 1990, year.t2 = 2012,
#'                                  methods = c('ARD', 'ARC'))
#' @export

getDeclineStats <- function (A.t1, A.t2, year.t1, year.t2,
                             methods){
  out <- data.frame(absolute.loss = (A.t1-A.t2))
  if(missing(methods)){
    stop("Please select method(s) to be used for calculating the rate of decline.")
  }
  if(any(methods == 'ARD')){
    ARD <- -((A.t2-A.t1)/(year.t2-year.t1))
    # Absolute rate of decline (also known as Annual Change(q)) in Puyrvaud
    out <- cbind(out, ARD = ARD)
  }
  if(any(methods == 'PRD')){
    PRD <- 100 * (1-(A.t2/A.t1)^(1/(year.t2-year.t1)))
    # Proportional rate of change (also known as trajectory (r))
    out <- cbind(out, PRD = PRD)
  }
  if(any(methods == 'ARC')){
    ARC <- (1/(year.t2-year.t1))*log(A.t2/A.t1) * 100
    # Annual rate of change from Puyravaud 2004 (also known as instantaneous rate of change)
    out <- cbind(out, ARC = ARC)
  }
  return (out)
}

#' Future Area Estimate
#'
#' `futureAreaEstimate` is now deprecated, please use
#' `extrapolateEstimate` instead
#'
#' @param A.t1 Area at time t1
#' @param year.t1 Year of time t1
#' @param nYears Number of years since t1 for area prediction
#' @param ARD Absolute rate of decline
#' @param PRD Proportional rate of decline
#' @param ARC Annual rate of change
#' @return A dataframe with the forecast year, and a combination of:
#' \itemize{
#'  \item Future area as estimated with absolute rate of decline (ARD)
#'  \item Future area as estimated with proportional rate of decline (PRD)
#'  \item Future area as estimated with annual rate of change (ARC)
#'  }
#' @author Nicholas Murray \email{murr.nick@@gmail.com}, Calvin Lee
#'   \email{calvinkflee@@gmail.com}
#' @family change_functions
#' @references Bland, L.M., Keith, D.A., Miller, R.M., Murray, N.J. and
#'   Rodriguez, J.P. (eds.) 2016. Guidelines for the application of IUCN Red
#'   List of Ecosystems Categories and Criteria, Version 1.0. Gland,
#'   Switzerland: IUCN. ix + 94pp. Available at the following web site:
#'   <https://iucnrle.org/>
#' @export

futureAreaEstimate <- function(A.t1, year.t1, nYears, ARD = NA, PRD = NA, ARC = NA){
  .Deprecated("extrapolateEstimate", "redlistr")
  extrapolateEstimate(A.t1 = A.t1, year.t1 = year.t1, nYears = nYears,
                      ARD = ARD, PRD = PRD, ARC = ARC)
}

#' Extrapolate Estimate
#'
#' `extrapolateEstimate` uses rates of decline from getDeclineStats
#' to extrapolate estimates to a given time
#'
#' @param A.t1 Area at time t1
#' @param year.t1 Year of time t1
#' @param nYears Number of years since t1 for prediction. Use negative
#' values for backcasting
#' @param ARD Absolute rate of decline
#' @param PRD Proportional rate of decline
#' @param ARC Annual rate of change
#' @return A dataframe with the forecast year, and a combination of:
#' \itemize{
#'  \item Values as extrapolated with absolute rate of decline (ARD)
#'  \item Values as extrapolated with proportional rate of decline (PRD)
#'  \item Values as extrapolated with annual rate of change (ARC)
#'  }
#' @author Nicholas Murray \email{murr.nick@@gmail.com}, Calvin Lee
#'   \email{calvinkflee@@gmail.com}
#' @family change_functions
#' @references Bland, L.M., Keith, D.A., Miller, R.M., Murray, N.J. and
#'   Rodriguez, J.P. (eds.) 2016. Guidelines for the application of IUCN Red
#'   List of Ecosystems Categories and Criteria, Version 1.0. Gland,
#'   Switzerland: IUCN. ix + 94pp. Available at the following web site:
#'   <https://iucnrle.org/>
#' @examples
#' a.r1 <- 23.55
#' a.r2 <- 15.79
#' decline.stats <- getDeclineStats(a.r1, a.r2, year.t1 = 1990, year.t2 = 2012,
#'                                  methods = 'PRD')
#' a.2040.PRD <- extrapolateEstimate(a.r1, a.r2, year.t1 = 1990, nYears = 50,
#'                                   PRD = decline.stats$PRD)
#' @export

extrapolateEstimate <- function(A.t1, year.t1, nYears, ARD = NA, PRD = NA, ARC = NA){
  y.t3 <- year.t1+nYears
  out <- data.frame(forecast.year = y.t3)
  if(!is.na(ARD)){
    A.ARD.t3 <- A.t1 - (ARD*nYears)
    if(A.ARD.t3 < 0) A.ARD.t3 = 0
    out <- cbind(out, A.ARD.t3 = A.ARD.t3)
  }
  if(!is.na(PRD)){
    A.PRD.t3 <- A.t1 * (1 -(PRD/100))^nYears
    if(A.PRD.t3 < 0) A.PRD.t3 = 0
    out <- cbind(out, A.PRD.t3 = A.PRD.t3)
  }
  if(!is.na(ARC)){
    A.ARC.t3 <- A.t1 * exp(ARC/100*nYears)
    if(A.ARC.t3 < 0) A.ARC.t3 = 0
    out <- cbind(out, A.ARC.t3 = A.ARC.t3)
  }
  if(all(c(is.na(PRD), is.na(ARD), is.na(ARC)))){
    stop("Please input at least one of 'ARD', 'PRD', or 'ARC'.")
  }
  return(out)
}

#' Sequential extrapolation estimate
#'
#' `sequentialExtrapolate` uses rates of decline from getDeclineStats and
#' generates a sequence of estimates at regular time-steps. Useful for
#' generating a sequence for plotting graphs.
#'
#' @inheritParams extrapolateEstimate
#'
#' @return A dataframe with the forecast year, and a combination of:
#' \itemize{
#'  \item Sequence of values as extrapolated with absolute rate of decline (ARD)
#'  \item Sequence of values as extrapolated with proportional rate of decline (PRD)
#'  \item Sequence of values as extrapolated with annual rate of change (ARC)
#'  }
#' @author Calvin Lee \email{calvinkflee@@gmail.com}
#' @family change_functions
#' @references Bland, L.M., Keith, D.A., Miller, R.M., Murray, N.J. and
#'   Rodriguez, J.P. (eds.) 2016. Guidelines for the application of IUCN Red
#'   List of Ecosystems Categories and Criteria, Version 1.0. Gland,
#'   Switzerland: IUCN. ix + 94pp. Available at the following web site:
#'   <https://iucnrle.org/>
#' @examples
#' a.r1 <- 23.55
#' a.r2 <- 15.79
#' decline.stats <- getDeclineStats(a.r1, a.r2, year.t1 = 1990, year.t2 = 2012,
#'                                  methods = 'PRD')
#' a.2040.PRD.seq <- sequentialExtrapolate(a.r1, a.r2, year.t1 = 1990, nYears = 50,
#'                                         PRD = decline.stats$PRD)
#' @export

sequentialExtrapolate <- function(A.t1, year.t1, nYears, ARD = NA, PRD = NA, ARC = NA){
  if(all(c(is.na(PRD), is.na(ARD), is.na(ARC)))){
    stop("Please input at least one of 'ARD', 'PRD', or 'ARC'.")
  }
  ARD_seq <- vector()
  PRD_seq <- vector()
  ARC_seq <- vector()
  for(i in 0:nYears){
    estimate <- extrapolateEstimate(A.t1,
                                    year.t1,
                                    nYears = i,
                                    ARD = ARD,
                                    PRD = PRD,
                                    ARC = ARC)

    ARD_seq <- c(ARD_seq, estimate$A.ARD.t3)
    PRD_seq <- c(PRD_seq, estimate$A.PRD.t3)
    ARC_seq <- c(ARC_seq, estimate$A.ARC.t3)
  }

  years <- seq(year.t1, sum(year.t1, nYears))

  # Dealing with empty vectors
  if(length(ARD_seq) == 0) ARD_seq <- NA
  if(length(PRD_seq) == 0) PRD_seq <- NA
  if(length(ARC_seq) == 0) ARC_seq <- NA

  out_df <- data.frame(years = years, ARD = ARD_seq,
                       PRD = PRD_seq, ARC = ARC_seq)
  return(out_df)
}
