# Calculates area of the created EOO polygon and returns a summary object with useful info.

`getEOO` calculates the area of the EOO polygon generated from `makeEOO`
the provided data and returns an EOO class object or a list of these
with defined summary and plot functions available.

## Usage

``` r
getEOO(input.data, names_from = NA)
```

## Arguments

- input.data:

  Spatial object of an ecosystem or species distribution. Please use a
  CRS with units measured in metres.

- names_from:

  name of the column containing ecosystem names. If missing all features
  will be analysed together. Only needed for vector data.

## Value

An object of type EOO or a list of EOO objects that store the EOO
polygon, its area, and its input.data

## See also

Other EOO functions:
[`getAreaEOO()`](http://red-list-ecosystem.github.io/redlistr/reference/getAreaEOO.md),
[`makeEOO()`](http://red-list-ecosystem.github.io/redlistr/reference/makeEOO.md)

## Author

Nicholas Murray <murr.nick@gmail.com>, Calvin Lee
<calvinkflee@gmail.com>

## Examples

``` r
m <- matrix(sample(1:4, 500, replace = TRUE, prob = c(4,1,1,6)), nrow=25, ncol=20)
r1 <- terra::rast(m, crs = "EPSG:32755")
EOO <- getEOO(r1)
```
