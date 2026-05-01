# Mangrove distribution data

Mangrove distribution from the northern regions of Western Port Bay,
Victoria, Australia, in the year 2000.

## Usage

``` r
a.2000
```

## Format

### `a.2000`

A data frame with 1 row and 3 columns:

- layer:

  name of the layer

- value:

  raster value from which area was computed

- area:

  area in km2

## Source

<https://onlinelibrary.wiley.com/doi/10.1111/j.1466-8238.2010.00584.x/abstract>
Mangrove distribution data

Mangrove distribution from the northern regions of Western Port Bay,
Victoria, Australia, in the year 2017.

computed from mangrove.2000 using getArea

## Details

The dataset is stored as a GeoTIFF file in `inst/extdata` and can be
loaded with:


    terra::rast(system.file("extdata", "example_distribution_2000.tif", package = "redlistr"))

The dataset is stored as a GeoTIFF file in `inst/extdata` and can be
loaded with:


    terra::rast(system.file("extdata", "example_distribution_2017.tif", package = "redlistr"))

Mangrove area

Area of mangroves in 2000
