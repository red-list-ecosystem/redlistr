# Decline forecasts

`declineForecast` calculates rates of change from two inputs and
extrapolates the rate of change to a desired future time interval.

## Usage

``` r
declineForecast(
  x,
  y,
  names_from_x = NA,
  names_from_y = NA,
  t1,
  year_diff,
  forecast_year = t1 + 50
)
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

- t1:

  numeric year of earliest dataset, corresponding to x

- year_diff:

  numeric year difference bewteen x and y inputs

- forecast_year:

  the desired year to which to forecast (or hindcast) change.

## Value

returns a list of two elements: a table of areas and change in areas and
a table of forecasts including decline rate, forecast area, and forecast
percent decline.

## See also

Other Change functions:
[`getArea()`](http://red-list-ecosystem.github.io/redlistr/reference/getArea.md),
[`getAreaChange()`](http://red-list-ecosystem.github.io/redlistr/reference/getAreaChange.md),
[`getAreaTrend()`](http://red-list-ecosystem.github.io/redlistr/reference/getAreaTrend.md),
[`getDeclineStats()`](http://red-list-ecosystem.github.io/redlistr/reference/getDeclineStats.md)

## Author

Aniko B. Toth <anikobtoth@gmail.com>
