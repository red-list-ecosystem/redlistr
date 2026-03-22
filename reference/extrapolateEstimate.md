# Extrapolate Estimate

`extrapolateEstimate` uses rates of decline from getDeclineStats to
extrapolate estimates to a given time

## Usage

``` r
extrapolateEstimate(A.t1, year.t1, nYears, ARD = NA, PRD = NA, ARC = NA)
```

## Arguments

- A.t1:

  Area at time t1

- year.t1:

  Year of time t1

- nYears:

  Number of years since t1 for prediction. Use negative values for
  backcasting

- ARD:

  Absolute rate of decline

- PRD:

  Proportional rate of decline

- ARC:

  Annual rate of change

## Value

A dataframe with the forecast year, and a combination of:

- Values as extrapolated with absolute rate of decline (ARD)

- Values as extrapolated with proportional rate of decline (PRD)

- Values as extrapolated with annual rate of change (ARC)

## References

IUCN 2024. Guidelines for the application of IUCN Red List of Ecosystems
Categories and Criteria, Version 2.0. Keith, D.A., Ferrer-Paris, J.R.,
Ghoraba, S.M.M., Henriksen, S., Monyeki, M., Murray, N.J., Nicholson,
E., Rowland, J., Skowno, A., Slingsby, J.A., Storeng, A.B.,
Valderrábano, M. & Zager, I. (Eds.) Gland, Switzerland: IUCN. ix + 94pp.
<https://doi.org/10.2305/CJDF9122>

## See also

Other change_functions:
[`futureAreaEstimate()`](http://red-list-ecosystem.github.io/redlistr/reference/futureAreaEstimate.md),
[`sequentialExtrapolate()`](http://red-list-ecosystem.github.io/redlistr/reference/sequentialExtrapolate.md)

## Author

Nicholas Murray <murr.nick@gmail.com>, Calvin Lee
<calvinkflee@gmail.com>

## Examples

``` r
a.r1 <- 23.55
a.r2 <- 15.79
decline.stats <- getDeclineStats(a.r1, a.r2, year.t1 = 1990, year.t2 = 2012,
                                 methods = 'PRD')
a.2040.PRD <- extrapolateEstimate(a.r1, a.r2, year.t1 = 1990, nYears = 50,
                                  PRD = decline.stats$PRD)
```
