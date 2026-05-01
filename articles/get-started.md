# Get started

## What is this package for?

`redlistr` is a package to assess spatial criteria of the [IUCN Red List
of Ecosystems](https://iucnrle.org/) (RLE) and [IUCN Red List of
Threatened Species](https://www.iucnredlist.org/) (RLTS).

The package has functions for calculating three metrics that are useful
for different subcriteria of RLE and RLTS.

| Metrics | main `redlistr` function | RLE subcriteria | RLTS subcriteria |
|----|----|----|----|
| Declines in distribution | `getDeclineStats` | A1, A2, A3 | A1c, A2c, A3c, A4c |
| Extent of Occurrence (EOO) | `getEOO` | B1 | B1 |
| Area of Occupancy (AOO) | `getAOO` | B2 | B2, D2 |

## Input data formats

Functions in this package expect spatial data as input. They should
represent a valid estimate of species or ecosystem distribution at one
or multiple points in time.

Since version 2.0.0 the `redlistr` functions will accept different
formats for the input vector and raster data. The spatial data need to
be read in R using functions from external packages, either `sf` or
`terra`:

| Data type | Example formats | R package | R class |
|----|----|----|----|
| Vector | Shapefile, Geopackage, GeoJSON, GDB | [`sf`](https://r-spatial.github.io/sf/) | `sf` |
| Vector | Shapefile, Geopackage, GeoJSON, GDB | [`terra`](https://rspatial.github.io/terra/) | `SpatVect` |
| Raster | GeoTIFF | [`terra`](https://rspatial.github.io/terra/) | `SpatRast` |

## Outputs of the functions

The output of the `redlistr` functions will be R objects with class
attributes. There are `print`, `summary`, and `plot` functions for these
classes.

## Using `redlistr`

A simple workflow for using `redlistr` consist of importing and
preparing the input data, running the main function and using one of the
helper functions to get the main information from the resulting object.

For example, suppose that the path of the spatial data is in a R
character variable names `raster_file` and the data already has a valid
Coordinate Reference System in meters. In this case these three lines of
code will create the EOO polygon and print the results of the area in
square kilometers.

``` r

ecosystem_map <- rast(raster_file)
ecosystem_EOO <- getEOO(ecosystem_map)
summary(ecosystem_EOO)
```

    ## example_distribution_2000_value_1 : EOO object
    ## ----------------------------
    ##  EOO area: 528.9541 square kms
    ##  CRS: WGS 84 / UTM zone 55S 
    ##  Input data class: SpatRaster
    ##  Input raster layers: 1
    ##  Raster dimensions: ext(339000, 372510, 5744990, 5774000)
    ## ----------------------------

Keep in mind that:

- Preparing the data is often the most time consuming task, and requires
  some knowledge of geospatial data and external packages (`sf` or
  `terra`).
- The main functions might require extra parameters, and the values for
  these might be different for species and ecosystems.
- Combinations of the helper functions are useful to check the validity
  of the results and summary statistics to use in the assessment.
- In some applications users might want to repeat assessment over
  multiple assessment units (different species or ecosystems)

## Elaborated examples

We provide different examples of the use of `redlistr` for risk
assessments for ecosystems and species.

The examples use different types of input data and focus on different
subcriteria.

| Example | Distribution data | Input format | Red list subcriteria |
|----|----|----|----|
| [Mangroves in Victoria, Australia](http://red-list-ecosystem.github.io/redlistr/articles/articles/redlistr-vignette.md) | Ecosystem distribution in 2000 and 2017 | Raster: GeoTiff | RLE A1, B1 and B2 |
| [Purple Copper butterfly, Australia](http://red-list-ecosystem.github.io/redlistr/articles/articles/species-vignette.md) | Species occurences | Vector (Points) from biodiversity portal | RLTS B1 and B2 |
| Tropical glaciers, Ecuador (in prep.) | Ecosystem distribution in year 2000 | Vector (Polygons): Geopackage | RLE B1 and B2 |
