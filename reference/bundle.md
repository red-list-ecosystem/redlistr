# Compute AOO and EOO, return as table

`bundle` performs AOO and EOO calculations on an input object and
returns the results as a table

## Usage

``` r
bundle(input_data, names_from = NA, ...)
```

## Arguments

- input_data:

  Spatial object of an ecosystem or species distribution. Please use a
  CRS with units measured in metres.

- names_from:

  name of the column containing ecosystem names. If missing all features
  will be analysed together. Only needed for vector data.

- ...:

  Additional graphical parameters passed to getAOO().

## Value

a data.frame containing AOO and EOO information for all input units as
rows.

## See also

Other synthesis functions:
[`list2table()`](http://red-list-ecosystem.github.io/redlistr/reference/list2table.md)

## Author

Aniko B. Toth <anikobtoth@gmail.com>
