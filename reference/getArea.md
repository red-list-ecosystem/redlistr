# Calculates the Area of ecosystems in spatial data

`getArea` reports the area of ecosystem units provided as spatial data

## Usage

``` r
getArea(x, names_from = NA, ...)
```

## Arguments

- x:

  A SpatRaster, SpatVector, or an sf object with POLYGONS geometry.

- names_from:

  a column names containing ecosystem labels, as a string or dplyr-style
  column name. Only required for SpatVector and sf types. Units are
  assumed to be delineated by raster value for SpatRasters.

- ...:

  Addition arguments based on input format

## Value

A data frame containing ecosystem identifiers and the total area of the
ecosystem units in x as a units vector (km^2). For raster bricks it also
contains the layer number.

## See also

Other Change functions:
[`getAreaChange()`](http://red-list-ecosystem.github.io/redlistr/reference/getAreaChange.md),
[`getAreaTrend()`](http://red-list-ecosystem.github.io/redlistr/reference/getAreaTrend.md),
[`getDeclineStats()`](http://red-list-ecosystem.github.io/redlistr/reference/getDeclineStats.md)

## Author

Nicholas Murray <murr.nick@gmail.com>, Calvin Lee
<calvinkflee@gmail.com>, Aniko B. Toth <anikobtoth@gmail.com>
