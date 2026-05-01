# Area change between two inputs in km2

`getAreaChange` reports the difference in area between two inputs.
Inputs can be SpatRaster, SpatVector, sf or a data frame of areas and
may contain data for multiple ecosystem types. Ensure x and y are the
same data type. If using data frame as input, ensure areas are measured
in km2

## Usage

``` r
getAreaChange(x, y, names_from_x = NA, names_from_y = NA)
```

## Arguments

- x:

  SpatRaster, SpatVector, or sf object representing one or more
  ecosystems or a data frame with two columns, one of them labeled
  "area", and the other containing ecosystem labels names_from_x.

- y:

  SpatRaster, SpatVector, or sf object representing one or more
  ecosystems or a data frame with two columns, one of them labeled
  "area", and the other containing ecosystem labels names_from_y.

- names_from_x:

  name of column containing ecosystem labels. Ignored if x is a raster.

- names_from_y:

  name of column containing ecosystem labels. Ignored if y is a raster.
  names_from_x used if not provided.

## Value

Returns a table containing ecosystem labels, areas, and the difference
in area of the two inputs in km2

## See also

Other Change functions:
[`getArea()`](http://red-list-ecosystem.github.io/redlistr/reference/getArea.md),
[`getAreaTrend()`](http://red-list-ecosystem.github.io/redlistr/reference/getAreaTrend.md),
[`getDeclineStats()`](http://red-list-ecosystem.github.io/redlistr/reference/getDeclineStats.md)

## Author

Nicholas Murray <murr.nick@gmail.com>, Calvin Lee
<calvinkflee@gmail.com>, Aniko B. Toth <anikobtoth@gmail.com>

## Examples

``` r
if (requireNamespace("terra", quietly = TRUE)) {
  ok <- try({
    m1 <- matrix(sample(1:4, 500, replace = TRUE, prob = c(4,1,1,6)), 25, 20)
    r1 <- terra::rast(m1)
    terra::crs(r1) <- "+proj=utm +zone=55 +south +datum=WGS84 +units=m +no_defs"

    m2 <- matrix(sample(1:4, 500, replace = TRUE, prob = c(4,1,1,6)), 25, 20)
    r2 <- terra::rast(m2)
    terra::crs(r2) <- "+proj=utm +zone=55 +south +datum=WGS84 +units=m +no_defs"

    getAreaChange(r1, r2)
  }, silent = TRUE)
}
```
