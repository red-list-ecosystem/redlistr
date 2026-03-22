
<!-- README.md is generated from README.Rmd. Please edit that file -->

# redlistr <img src="man/figures/logo.png" align="right" height="138" alt="Red List of Ecosystems logo" />

`redlistr` is an R package that contains functions for calculating the
metrics required for making risk assessments of species and ecosystems
according to the IUCN Red List of Threatened Species and the IUCN Red
List of Ecosystems categories and criteria.The paper describing
`redlistr` has been published on Ecography and is available
[here](https://onlinelibrary.wiley.com/doi/full/10.1111/ecog.04143).

## Overview

The `redlistr` package was developed to assist users conduct assessments
for the IUCN Red List of Ecosystems in `R`. It is also useful for users
interested in conducting assessments for the Red List of Threatened
Species. Assessments of ecosystems under the IUCN Red List of Ecosystems
criteria require calculation of standardised metrics that were developed
to objectively assess risk to ecosystems ([Keith et al.
2013](https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0062111)).
This package was designed to assist in the calculation of these metrics,
including two methods of calculating the rate of distributional decline:
Absolute Rate of Decline (ARD) and Proportional Rate of Decline (PRC).
An additional metric: the Annual Rate of Change (ARC), which uses a
compound interest law to determine the instantaneous rate of change
([Puyravaud
2003](https://www.sciencedirect.com/science/article/pii/S0378112702003353))
is also included.

Also included are the two standard measures of the size of an
ecosystems’ geographic distribution specified in the Red List of
Ecosystems guidelines ([IUCN 2024](https://doi.org/10.2305/CJDF9122))
and the Red List of Threatened Species guidelines ([IUCN Standards and
Petitions Committee
2024](https://www.iucnredlist.org/resources/redlistguidelines)). These
are the Extent of Occurrence (EOO) and Area of Occupancy (AOO).

In conducting an assessment with this package, we assume that you are
familiar with IUCN red listing protocols. In particular, you should
consult the IUCN guidelines for each of the red lists, which are the
definitive sources of all information required to ensure consistent
application of IUCN criteria ([IUCN
2024](https://doi.org/10.2305/CJDF9122)). In addition, the papers by
Keith et al.
([2013](https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0062111))
and Rodriguez et al. ([2015](https://doi.org/10.1098/rstb.2014.0003))
are particularly useful for navigating the IUCN Red List of Ecosystems
criteria. A range of important resources, including software tools and
guiding material is available on the IUCN Red List of Ecosystems
[website](https://www.iucnrle.org/). There is also plenty of handy
material for assessing species available on the IUCN Red List of
Threatened Species [website](https://www.iucnredlist.org).

We also assume that you are reasonably familiar with the `R` programming
language, and have some experience in conducting analyses of vector and
raster data within the `R` environment. Of particular use will be
familiarity with the `terra` and `sf` packages. This is certainly not a
prerequisite, but this package is built upon many of the functions
available in these two packages.

For a more detailed tutorial explaining how to use this package, please
refer to the vignettes available with the package.

Lastly, this is a work in progress and we aim to continually add new
functions to newer versions of package. Suggestions are welcomed, as are
offers for collaborative development.

## Installation

You can install the development version from GitHub.

``` r
# install.packages("devtools")
devtools::install_github("red-list-ecosystem/redlistr")
```

An older version of `redlistr` is available on CRAN (v 1.0.4). The plan
is to release the new version in CRAN during 2026.

``` r
install.packages("redlistr")
```

> important note: [rredlist](https://github.com/ropensci/rredlist) is a
> different package that works with the IUCN Red List of Threatened
> Species’ API.
