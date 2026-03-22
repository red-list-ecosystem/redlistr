# Fit linear model with log-link.

`fit_lm_ll` produces a simple model to estimate trends in short time
series.

## Usage

``` r
fit_lm_ll(df)

fit_glm_ll(df)

fit_spline(df)
```

## Arguments

- df:

  a data frame containing the columns "area" and "t" for time. Time can
  be an index or year.

## Value

an lm object

## Author

Aniko B. Toth <anikobtoth@gmail.com>
