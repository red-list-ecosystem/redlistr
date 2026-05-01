# Create empty Area of Occupancy (AOO) Grid.

`createGrid` produces empty grid which can be used as the basis to help
compute AOO.

## Usage

``` r
createGrid(input_data, cell_size = 10000)
```

## Arguments

- input_data:

  Spatial object (sf or SpatRaster) of an ecosystem or species
  distribution. Please use a CRS with units measured in metres.

- cell_size:

  A number specifying the width of the desired grid square (in same
  units as your coordinate reference system)

## Value

A regular grid raster with extent `input_data` expanded by two cells in
each direction and grid size `cell_size`. Each grid square has a unique
raster value that serves as its identification number.

## References

Bland, L.M., Keith, D.A., Miller, R.M., Murray, N.J. and Rodriguez, J.P.
(eds.) 2016. Guidelines for the application of IUCN Red List of
Ecosystems Categories and Criteria, Version 1.0. Gland, Switzerland:
IUCN. ix + 94pp. Available at the following web site:
<https://iucnrle.org/>

## See also

Other AOO functions:
[`getAOO()`](http://red-list-ecosystem.github.io/redlistr/reference/getAOO.md),
[`jplot()`](http://red-list-ecosystem.github.io/redlistr/reference/jplot.md),
[`makeAOOGrid()`](http://red-list-ecosystem.github.io/redlistr/reference/makeAOOGrid.md),
[`top_pct()`](http://red-list-ecosystem.github.io/redlistr/reference/top_pct.md)

## Author

Nicholas Murray <murr.nick@gmail.com>, Calvin Lee
<calvinkflee@gmail.com>, Aniko B. Toth <anikobtoth@gmail.com>
