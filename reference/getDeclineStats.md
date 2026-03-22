# Change statistics.

`getDeclineStats` calculates the Proportional Rate of Decline (PRD),
Absolute Rate of Decline (ARD) and Annual Rate of Change (ARC), given
two areas at two points in time. Also provides the total area
difference. Inputs are usually the results from `getArea`.

## Usage

``` r
getDeclineStats(A.t1, A.t2, year.t1, year.t2, methods)
```

## Arguments

- A.t1:

  Area at time t1

- A.t2:

  Area at time t2

- year.t1:

  Year of time t1

- year.t2:

  Year of time t2

- methods:

  Method(s) used to calculate rate of decline. Either 'PRD', 'ARD',
  and/or 'ARC'. See vignette to see a more detailed explanation for each
  of them.

## Value

A dataframe with absolute differences between the two inputs, and a
selection of:

- Proportional Rate of Decline (PRD)

- Absolute Rate of Decline (ARD)

- Annual Rate of Change (ARC)

## References

IUCN 2024. Guidelines for the application of IUCN Red List of Ecosystems
Categories and Criteria, Version 2.0. Keith, D.A., Ferrer-Paris, J.R.,
Ghoraba, S.M.M., Henriksen, S., Monyeki, M., Murray, N.J., Nicholson,
E., Rowland, J., Skowno, A., Slingsby, J.A., Storeng, A.B.,
Valderrábano, M. & Zager, I. (Eds.) Gland, Switzerland: IUCN. ix + 94pp.
<https://doi.org/10.2305/CJDF9122> Puyravaud, J.-P. 2003. Standardizing
the calculation of the annual rate of deforestation. Forest Ecology and
Management, 177, 593-596.

## See also

Other Change functions:
[`getArea()`](http://red-list-ecosystem.github.io/redlistr/reference/getArea.md),
[`getAreaChange()`](http://red-list-ecosystem.github.io/redlistr/reference/getAreaChange.md),
[`getAreaTrend()`](http://red-list-ecosystem.github.io/redlistr/reference/getAreaTrend.md)

## Author

Nicholas Murray <murr.nick@gmail.com>, Calvin Lee
<calvinkflee@gmail.com>

## Examples

``` r
a.r1 <- 23.55
a.r2 <- 15.79
decline.stats <- getDeclineStats(a.r1, a.r2, year.t1 = 1990, year.t2 = 2012,
                                 methods = c('ARD', 'ARC'))
```
