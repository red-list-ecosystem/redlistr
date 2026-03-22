# Make elbow plot to check jitter iterations

`jplot` creates an elbow plot of the min AOO against the number of grid
replicates run. Jplots that fall to the minimum value well before the
highest n are robust.

## Usage

``` r
jplot(x)
```

## Arguments

- x:

  an AOOgrid object

## Value

NULL; plots min AOO against number of reps

## See also

Other AOO functions:
[`createGrid()`](http://red-list-ecosystem.github.io/redlistr/reference/createGrid.md),
[`getAOO()`](http://red-list-ecosystem.github.io/redlistr/reference/getAOO.md),
[`makeAOOGrid()`](http://red-list-ecosystem.github.io/redlistr/reference/makeAOOGrid.md),
[`top_pct()`](http://red-list-ecosystem.github.io/redlistr/reference/top_pct.md)

## Author

Aniko B. Toth <anikobtoth@gmail.com>
