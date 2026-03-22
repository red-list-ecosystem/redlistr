# Calculate area trends of ecosystems

`getAreaTrend` is used to calculate changes in area over time. Output is
a list can be used to easily visualise trends for one ecosystem at a
time.

## Usage

``` r
getAreaTrend(x, names_from = NA)
```

## Arguments

- x:

  SpatRaster with multiple layers or data frame containing two numerical
  columns "area" and "time". The data.frame can contain an additional
  column containing a key, whose name should be placed in names_from.

- names_from:

  name of column containing ecosystem labels. Ignored if x is a raster.

## Value

returns a list

## See also

Other Change functions:
[`getArea()`](http://red-list-ecosystem.github.io/redlistr/reference/getArea.md),
[`getAreaChange()`](http://red-list-ecosystem.github.io/redlistr/reference/getAreaChange.md),
[`getDeclineStats()`](http://red-list-ecosystem.github.io/redlistr/reference/getDeclineStats.md)

## Author

Aniko B. Toth <anikobtoth@gmail.com>
