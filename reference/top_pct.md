# Identify positions of the bottom 1% of ecosystem area in AOO grid

`top_pct` returns the vector positions of the largest elements
collectively comprising a given percentage more of the vector sum. This
function helps perform the bottom.1pct.rule when selecting the AOO grid
by identifying grid positions to keep.

## Usage

``` r
top_pct(v, pct = 99)
```

## Arguments

- v:

  A numeric vector.

- pct:

  percent of area to drop

## Value

a numeric vector indicating the indeces of the elements to keep.

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
[`getAOO()`](http://red-list-ecosystem.github.io/redlistr/reference/getAOO.md),
[`jplot()`](http://red-list-ecosystem.github.io/redlistr/reference/jplot.md),
[`makeAOOGrid()`](http://red-list-ecosystem.github.io/redlistr/reference/makeAOOGrid.md)

## Author

Aniko B. Toth <anikobtoth@gmail.com>
