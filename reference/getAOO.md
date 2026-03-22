# Compute Area of Occupancy (AOO)

`getAOO` determines the number of area of occupancy (AOO) grid cells
occupied by a species or ecosystem. It includes capability for
specifying whether at least one percent of the grid cell needs to be
occupied before it is counted in the AOO. This functionality is
important for assessing the IUCN Red List of Ecosystems Criteria B.

## Usage

``` r
getAOO(
  input.data,
  grid.size = 10000,
  names_from = NA,
  bottom.1pct.rule = TRUE,
  percent = 1,
  jitter = TRUE,
  n_jitter = 35
)
```

## Arguments

- input.data:

  Spatial object (sf or SpatRaster) of an ecosystem or species
  distribution. Please use a CRS with units measured in metres.

- grid.size:

  A number specifying the width of the desired grid square (in same
  units as your coordinate reference system)

- names_from:

  the name of the column containing ecosystem labels

- bottom.1pct.rule:

  Logical. If `TRUE`, grid cells containing the least ecosystem area are
  dropped up to 1% of the total distribution.

- percent:

  Numeric. The minimum percent to be applied as a threshold for the
  `bottom.1pct.rule`

- jitter:

  logical. Whether grid randomization should be applied to units with
  low grid counts.

- n_jitter:

  the number of grids to test for ecosystems near the AOO thresholds.
  Ignored if jitter = FALSE.

## Value

an object of class AOOgrid or a list of AOOgrid objects. Ecosystems that
received an AOO of 60 cells or fewer on a first pass are run with a
jittered grid with n specified by n_jitter

## References

IUCN 2024. Guidelines for the application of IUCN Red List of Ecosystems
Categories and Criteria, Version 2.0. Keith, D.A., Ferrer-Paris, J.R.,
Ghoraba, S.M.M., Henriksen, S., Monyeki, M., Murray, N.J., Nicholson,
E., Rowland, J., Skowno, A., Slingsby, J.A., Storeng, A.B.,
Valderrábano, M. & Zager, I. (Eds.) Gland, Switzerland: IUCN. ix + 94pp.
<https://doi.org/10.2305/CJDF9122>

## See also

Other AOO functions:
[`createGrid()`](http://red-list-ecosystem.github.io/redlistr/reference/createGrid.md),
[`jplot()`](http://red-list-ecosystem.github.io/redlistr/reference/jplot.md),
[`makeAOOGrid()`](http://red-list-ecosystem.github.io/redlistr/reference/makeAOOGrid.md),
[`top_pct()`](http://red-list-ecosystem.github.io/redlistr/reference/top_pct.md)

## Author

Nicholas Murray <murr.nick@gmail.com>, Calvin Lee
<calvinkflee@gmail.com>, Aniko B. Toth <anikobtoth@gmail.com>
