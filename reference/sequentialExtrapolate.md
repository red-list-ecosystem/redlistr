# Sequential extrapolation estimate

`sequentialExtrapolate` uses rates of decline from getDeclineStats and
generates a sequence of estimates at regular time-steps. Useful for
generating a sequence for plotting graphs.

## Usage

``` r
sequentialExtrapolate(A.t1, year.t1, nYears, ARD = NA, PRD = NA, ARC = NA)
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

- Sequence of values as extrapolated with absolute rate of decline (ARD)

- Sequence of values as extrapolated with proportional rate of decline
  (PRD)

- Sequence of values as extrapolated with annual rate of change (ARC)

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
[`futureAreaEstimate()`](http://red-list-ecosystem.github.io/redlistr/reference/futureAreaEstimate.md)

## Author

Calvin Lee <calvinkflee@gmail.com>

## Examples

``` r
a.r1 <- 23.55
a.r2 <- 15.79
decline.stats <- getDeclineStats(a.r1, a.r2, year.t1 = 1990, year.t2 = 2012,
                                 methods = 'PRD')
a.2040.PRD.seq <- sequentialExtrapolate(a.r1, a.r2, year.t1 = 1990, nYears = 50,
                                        PRD = decline.stats$PRD)
```
