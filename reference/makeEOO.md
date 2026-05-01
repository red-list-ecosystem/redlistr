# Creates Extent of occurrence (EOO) Polygon

`makeEOO` is a generic function that creates a minimum convex polygon
enclosing all occurrences of the ecosystems provided in the input data.
If the input provided is a raster layer, the points are taken from a
buffer that has the radius of half of the shorter edge of the pixel
around the centroid.

## Usage

``` r
makeEOO(input_data, names_from)
```

## Arguments

- input_data:

  Spatial object of an ecosystem or species distribution. Please use a
  CRS with units measured in metres.

- names_from:

  name of the column containing ecosystem names. If missing all features
  will be analysed together. Only needed for vector data.

## Value

An object of class sf representing the EOO of `input_data`, or a list of
sf objects if multiple ecosystems were input. Also inherits its CRS from
input_data.

## References

IUCN 2024. Guidelines for the application of IUCN Red List of Ecosystems
Categories and Criteria, Version 2.0. Keith, D.A., Ferrer-Paris, J.R.,
Ghoraba, S.M.M., Henriksen, S., Monyeki, M., Murray, N.J., Nicholson,
E., Rowland, J., Skowno, A., Slingsby, J.A., Storeng, A.B.,
Valderrábano, M. & Zager, I. (Eds.) Gland, Switzerland: IUCN. ix + 94pp.
<https://doi.org/10.2305/CJDF9122>

## See also

Other EOO functions:
[`getAreaEOO()`](http://red-list-ecosystem.github.io/redlistr/reference/getAreaEOO.md),
[`getEOO()`](http://red-list-ecosystem.github.io/redlistr/reference/getEOO.md)

## Author

Nicholas Murray <murr.nick@gmail.com>, Calvin Lee
<calvinkflee@gmail.com>

## Examples

``` r
if (requireNamespace("terra", quietly = TRUE)) {
  ok <- try({
     m <- matrix(sample(1:4, 500, replace = TRUE, prob = c(4,1,1,6)), nrow=25, ncol=20)
     r1 <- terra::rast(m, crs = "+proj=utm +zone=55 +south +datum=WGS84 +units=m +no_defs")
     EOO.polygon <- makeEOO(r1)
  }, silent = TRUE)
}
```
