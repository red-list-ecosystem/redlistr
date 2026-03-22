# Future Area Estimate

`futureAreaEstimate` is now deprecated, please use `extrapolateEstimate`
instead

## Usage

``` r
futureAreaEstimate(A.t1, year.t1, nYears, ARD = NA, PRD = NA, ARC = NA)
```

## Arguments

- A.t1:

  Area at time t1

- year.t1:

  Year of time t1

- nYears:

  Number of years since t1 for area prediction

- ARD:

  Absolute rate of decline

- PRD:

  Proportional rate of decline

- ARC:

  Annual rate of change

## Value

A dataframe with the forecast year, and a combination of:

- Future area as estimated with absolute rate of decline (ARD)

- Future area as estimated with proportional rate of decline (PRD)

- Future area as estimated with annual rate of change (ARC)

## References

IUCN 2024. Guidelines for the application of IUCN Red List of Ecosystems
Categories and Criteria, Version 2.0. Keith, D.A., Ferrer-Paris, J.R.,
Ghoraba, S.M.M., Henriksen, S., Monyeki, M., Murray, N.J., Nicholson,
E., Rowland, J., Skowno, A., Slingsby, J.A., Storeng, A.B.,
Valderrábano, M. & Zager, I. (Eds.) Gland, Switzerland: IUCN. ix + 94pp.
<https://doi.org/10.2305/CJDF9122>

## See also

Other change_functions:
[`extrapolateEstimate()`](http://red-list-ecosystem.github.io/redlistr/reference/extrapolateEstimate.md),
[`sequentialExtrapolate()`](http://red-list-ecosystem.github.io/redlistr/reference/sequentialExtrapolate.md)

## Author

Nicholas Murray <murr.nick@gmail.com>, Calvin Lee
<calvinkflee@gmail.com>
