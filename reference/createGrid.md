# Create empty Area of Occupancy (AOO) Grid.

`createGrid` produces empty grid which can be used as the basis to help
compute AOO.

## Usage

``` r
createGrid(input.data, grid.size = 10000)
```

## Arguments

- input.data:

  Spatial object (sf or SpatRaster) of an ecosystem or species
  distribution. Please use a CRS with units measured in metres.

- grid.size:

  A number specifying the width of the desired grid square (in same
  units as your coordinate reference system)

## Value

A regular grid raster with extent `input.data` expanded by two cells in
each direction and grid size `grid.size`. Each grid square has a unique
raster value that serves as its identification number.

## References

IUCN 2024. Guidelines for the application of IUCN Red List of Ecosystems
Categories and Criteria, Version 2.0. Keith, D.A., Ferrer-Paris, J.R.,
Ghoraba, S.M.M., Henriksen, S., Monyeki, M., Murray, N.J., Nicholson,
E., Rowland, J., Skowno, A., Slingsby, J.A., Storeng, A.B.,
Valderrábano, M. & Zager, I. (Eds.) Gland, Switzerland: IUCN. ix + 94pp.
<https://doi.org/10.2305/CJDF9122>

## See also

Other AOO functions:
[`getAOO()`](http://red-list-ecosystem.github.io/redlistr/reference/getAOO.md),
[`jplot()`](http://red-list-ecosystem.github.io/redlistr/reference/jplot.md),
[`makeAOOGrid()`](http://red-list-ecosystem.github.io/redlistr/reference/makeAOOGrid.md),
[`top_pct()`](http://red-list-ecosystem.github.io/redlistr/reference/top_pct.md)

## Author

Nicholas Murray <murr.nick@gmail.com>, Calvin Lee
<calvinkflee@gmail.com>, Aniko B. Toth <anikobtoth@gmail.com>
