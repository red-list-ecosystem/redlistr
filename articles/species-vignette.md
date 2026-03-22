# Calculating spatial metrics for IUCN RLTS assessments

## Introduction

In this vignette, we provide an example of using the tools within
`redlistr` to assess two criteria of the [IUCN Red List of Threatened
Species](https://www.iucnredlist.org).

We will use a real life example and work through four steps that are
necessary to evaluate these criteria using R.

In this vignette we will start with some context about our target
species, then we will import a selection of R packages that will be
helpful for different tasks, and then we proceed with the four steps of:
data import, data preparation, calculation of metrics and application of
thresholds.

## The purple copper butterfly in Australia

*Paralucia spinifera* Edwards and Common, 1978 is a small butterfly in
the Lycaenidae Family. It has a purple and green iridescent upper
surface and muted brown, black and grey undersides.

This species is known only from a small number of sites east of
Bathurst. Its distribution is believed to be restricted by abiotic
factos (locations above 900m elevation) and biotic interactions with a
specific host plant (*Bursaria spinosa*) and attendant ant
(*Anonychomyrma itinerans*.

Adults fly from late September to late November. Eggs are laid singly or
in small groups. Eggs hatch in 14-17 days and the larval period lasts
6-8 weeks. Pupation takes place between December to late February in
ants nests at the base of the host plant; pupae remain in the ants nest
until the following spring.

The survival of Paralucia spinifera is threatened by habitat disturbance
like grazing of the host plant by sheep, goats and cattle, clearing for
the establishment of pine plantations, invasion of exotic weeds and
activities of feral pigs.

The latest conservation assessment was conducted in 2024:

Conservation Assessment of the purple copper butterfly Paralucia
spinifera Edwards and Common, 1978 (Lycaenidae) Matt Saunders 15/08/2024
Conservation Policy and Programs Division NSW Department of Climate
Change, Energy, the Environment and Water.
[link](https://www.environment.nsw.gov.au/topics/animals-and-plants/threatened-species/nsw-threatened-species-scientific-committee/nsw-threatened-species-scientific-committee-publications/assessment-reports)

Paralucia spinifera was found to be Endangered under IUCN Criterion
B1b(ii,iii,iv,v)c(iv)+2b(ii,iii,iv,v)c(iv).

## Loading packages in R

We begin by loading the packages we need for data import and for
calculation of the spatial metrics. Please ensure that these are already
installed.

For the first step of data download we will use the `galah` package that
allows downloading data from several repositories of biodiversity
records, including the Atlas of Living Australia.

For the second step of data preparation we will use useful data
wrangling packages `dplyr` and `lubridate`. We will use the package `sf`
to transform the records into an spatial object, and `mapview` to
inspect them.

Finally, we will use `redlistr` to calculate the spatial metrics for the
assessment. We also use functions from the `units` package to calculate
areas in different units.

``` r
library(galah)
library(sf)
library(mapview)
library(dplyr)
library(lubridate)
library(units)
library(redlistr)
```

## Step 1: Download the data

We configure the `galah` package to query
[ALA](http://red-list-ecosystem.github.io/redlistr/articles/www.ala.org.au)
and provide basic login information. You will need to replace
`<YOUR USER NAME>` and `<YOUR EMAIL>` with the values you would use in
the ALA portal.

``` r
galah_config(atlas = "Australia",
             username = "<YOUR USER NAME>",
             email = "<YOUR EMAIL>")
```

We make a first query to look at the number of records per data
resource. We construct this query using a sequence of function calls
concatenated by a “pipe” (`|>`):

``` r
galah_call() |>
        identify("Paralucia spinifera") |>
        group_by(dataResourceName) |>
        atlas_counts()
```

    ## # A tibble: 10 × 2
    ##    dataResourceName                                  count
    ##    <chr>                                             <int>
    ##  1 NSW BioNet Atlas                                    923
    ##  2 iNaturalist Australia                                54
    ##  3 Butterflies Australia                                27
    ##  4 Museum of Comparative Zoology, Harvard University    17
    ##  5 Western Australian Museum provider for OZCAM          6
    ##  6 Lepidoptera specimen database of Japan                5
    ##  7 INSDC Sequences                                       4
    ##  8 NSW DPI Biosecurity Collections                       2
    ##  9 ALA species sightings and OzAtlas                     1
    ## 10 BowerBird                                             1

We want to replicate the methods used in the most recent assessment. We
construct a query to download records from the *NSW BioNet Atlas* and
*iNaturalist Australia* and to only include records prior to 2024:

``` r
selected <- c("NSW BioNet Atlas", "iNaturalist Australia ")
ala_records <- galah_call() |>
        identify("Paralucia spinifera") |>
        filter(
            dataResourceName %in% selected,
            year < 2024) |>
        atlas_occurrences()
```

    ## --

We want to check that were working with valid presence records:

``` r
ala_records |> 
    group_by(basisOfRecord, occurrenceStatus) |> 
    summarise(n(), .groups="drop")
```

    ## # A tibble: 1 × 3
    ##   basisOfRecord     occurrenceStatus `n()`
    ##   <chr>             <chr>            <int>
    ## 1 HUMAN_OBSERVATION PRESENT            827

That looks fine, we are ready for the next step.

**Note**: Keep in mind that many biodiversity repositories have rules
for sharing coordinates of threatened species. In those cases the
coordinates might be obscured. If that is the case for the species of
interest, you will need to follow an extra procedure to get access to
the raw coordinates of the records.

## Step 2: Prepare the data

The downloaded dataset has few columns with the basic record
information:

``` r
glimpse(ala_records)
```

    ## Rows: 827
    ## Columns: 9
    ## $ recordID         <chr> "000f1f2c-8005-48ee-a85f-306ff2f360cd", "0027d150-6e9…
    ## $ scientificName   <chr> "Paralucia spinifera", "Paralucia spinifera", "Paralu…
    ## $ taxonConceptID   <chr> "https://biodiversity.org.au/afd/taxa/038c39ed-1c57-4…
    ## $ decimalLatitude  <dbl> -33.32353, -33.51477, -33.32153, -33.50470, -33.52182…
    ## $ decimalLongitude <dbl> 149.8152, 150.1435, 149.8145, 149.8210, 150.1508, 150…
    ## $ eventDate        <dttm> 2020-09-12, 2023-10-06, 2020-09-28, 2019-09-27, 2021…
    ## $ basisOfRecord    <chr> "HUMAN_OBSERVATION", "HUMAN_OBSERVATION", "HUMAN_OBSE…
    ## $ occurrenceStatus <chr> "PRESENT", "PRESENT", "PRESENT", "PRESENT", "PRESENT"…
    ## $ dataResourceName <chr> "NSW BioNet Atlas", "NSW BioNet Atlas", "NSW BioNet A…

We will be working with dates and geospatial coordinates, so we need to
do extra steps to transform these columns to special data formats.

First, we use the function `st_as_sf` to convert the table of records
into a spatial points dataset. We specify the columns with the
coordinates and the coordinate reference system information
corresponding to unprojected geographic coordinates.

Second, we use the `mutate` and `year` functions to extract the year
from the date string

``` r
ala_points <- st_as_sf(ala_records,
    coords = c("decimalLongitude", "decimalLatitude"),
    crs = "EPSG:4326") |>
    mutate(Year=year(eventDate))
```

We want to verify the accuracy of the records we are using, and exclude
any dubious records. Erroneous records can arise from misidentifications
in the field, mislabeling in the original source or the collection, or
by errors introducing during and after digitalisation of the record.

In real life cases, there should be a comprehensive verification from
the source of the raw data to the digital record to flag and correct
errors. For this excercise we will restrict ourselves to the filtering
of spatial outliers that we classify as dubious records.

We can visualise the records and check for obvious outliers. With the
interactive interface of `mapview` we can click on suspicious points and
get the `recordIDs`:

``` r
mapview(ala_points, zcol = "Year")
```

I suspect that these two records are dubious:

``` r
dubious_records <-  c(
    "7a782f5c-55a5-4863-9644-15e2e3ed79c5",
    "a8f7ab6f-45aa-47bf-a522-a28853ef25e0"
)
```

We can apply the filter function to the spatial dataset in order to get
a cleaned list of record:

``` r
cleaned_points <- ala_points |>
    filter(!recordID %in% dubious_records)
```

Now we have cleaned the dataset but for the next step we still need to
do one more task. The spatial functions of the `redlistr` package
require the dataset to be projected in an appropriate coordinate
reference system that allow area calculations.

We can use the function `st_transform` to reproject the data:

``` r
projected_points <- 
    st_transform(cleaned_points,
    "EPSG:3112")
```

Now we can do the next step.

## Step 3: Calculate the metrics

Our data is in a format that can be directly used by the functions in
the `redlistr` package.

The `getEOO` function will create a convex hull around the presence
records:

``` r
best_EOO <- getEOO(projected_points)
```

And we can summarise the results using the `summary` function:

``` r
summary(best_EOO)
```

    ## Summary of EOO object
    ## ----------------------------
    ## EOO area: 1,566.704 square kms
    ## Polygon geometry type(s): POLYGON
    ## Number of polygon features: 1
    ## Input data class: sf
    ## Input feature count: 825

We might want to recalculate EOO using a subset of records. For example,
this species shows fluctuations in population size and might undergo
local extinctions. Older records without recent resightings might
inflate the EOO.

Here we filter the data, recalculate the EOO and call the summary
function in one chunk of code:

``` r
projected_points |>
    filter(Year>2010) |>
    getEOO() |>
    summary()
```

    ## Summary of EOO object
    ## ----------------------------
    ## EOO area: 1,414.607 square kms
    ## Polygon geometry type(s): POLYGON
    ## Number of polygon features: 1
    ## Input data class: sf
    ## Input feature count: 704

This example shows that the value of EOO is sensitive to the assumption
that all historical records represent the current extent of the species.
By excluding older occurrence records we are trying to account for
potential local extinctions in the estimate of EOO.

Next we calculate the Area of Occupancy with the function `getAOO`. For
species assessments we use a grid size of 2000 m and we don’t want to
use the 1% rule.

``` r
simple_AOO <- getAOO(projected_points, 
    grid.size=2000,
    bottom.1pct.rule = FALSE,
    jitter = FALSE)
```

    ## Initialising grids

    ## Assembling initial grids

The `summary` function provides the results in number of grid cells:

``` r
summary(simple_AOO)
```

    ## Class: AOO_grid
    ## Number of grid cells: 43 
    ## Grid extent:
    ##     xmin     ymin     xmax     ymax 
    ##  1432266 -3903713  1492458 -3844983 
    ## CRS: GDA94 / Geoscience Australia Lambert 
    ## 
    ## function call parameters:
    ## grid size:  2000 
    ## jitter:  FALSE 
    ## n_jitter:

The value of the AOO is known to change depending on the placing of the
grid. We can use an option called `jitter` to recalculate the AOO
multiple times and summarise the range of potential values of AOO.

``` r
best_AOO <- getAOO(projected_points, 
    grid.size=2000,
    bottom.1pct.rule = FALSE,
    jitter = TRUE,
    n_jitter = 99
    )
```

    ## Initialising grids

    ## Assembling initial grids

    ## Running jitter on units with 100 or fewer cells, n = 99

    ## jittering

We can use this code to calculate the result in area of occupancy by
multiplying the number of cells with their area in square km.

``` r
gridsize <- set_units(best_AOO@params$gridsize,'m')^2 |> set_units('km2')
AOOvals <- best_AOO@AOOvals * gridsize
summary(AOOvals)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##     152     164     168     168     172     196

With the use of the jitter option, we get values between 148 and 188
${km}^{2}$.

Similarly to the EOO, we might want to recalculate the AOO using subsets
of the data. But we will leave this as an excercise for the reader.

Finally, we can now visualise the results together with the input data
to verify that they are all consistent. We use again the interactive
functions in the `mapview` package:

``` r
mapview(projected_points, hide=TRUE) +
mapview(best_AOO@grid) +
mapview(best_EOO@pol)
```

## Step 4: Apply thresholds

Criterion B of the IUCN Red List of Threatened Species is based on the
geographic range in the form of either extent of occurrence (B1) or area
of occupancy (B2) combined with some conditions that make the species
prone to abrupt changes.

These criteria have different quantitative thresholds for each category:

| Category              | Extent of occurrence (EOO) | Area of occupancy (AOO) | Other Conditions                                                                                    |
|-----------------------|----------------------------|-------------------------|-----------------------------------------------------------------------------------------------------|
| Vulnerable            | \<20,000 ${km}^{2}$        | \< 2,000 ${km}^{2}$     | at least 2 of: a) fragmented or \<= 10 locations, B) continuing decline, or c) extreme fluctuations |
| Endangered            | \<5,000 ${km}^{2}$         | \< 500 ${km}^{2}$       | at least 2 of: a) fragmented or \<= 5 locations, B) continuing decline, or c) extreme fluctuations  |
| Critically Endangered | \<100 ${km}^{2}$           | \< 10 ${km}^{2}$        | at least 2 of: a) fragmented or one locations, B) continuing decline, or c) extreme fluctuations    |

Our calculation found that the EOO is above 100 ${km}^{2}$ but below
5,000 ${km}^{2}$. From the previous assessment we also know that the
species meets the condition of a severely fragmented distribution, and
there is evidence of inferred declines in area, extent and/or quality of
habitat, and extreme fluctuations in the number of mature individuals.
The outcome would be a category of Endangered.

Similarly, our calculation resulted in an AOO above 10 ${km}^{2}$ but
below 500 ${km}^{2}$. From the previous assessment we also know that the
species meets the condition of a severely fragmented distribution, and
there is evidence of inferred declines in area, extent and/or quality of
habitat, and extreme fluctuations in the number of mature individuals.
The outcome would be a category of Endangered.

## Conclusions

We will compare the outputs of this assessment with the existing
assessment for this species:

The values of AOO reported in the conservation assessment of the species
was 152 — 176 ${km}^{2}$ and the extent of occurrence (EOO) is 1,693 —
1,823 ${km}^{2}$. The AOO was based on 2 x 2 km grid cells, and the EOO
is based on a minimum convex polygon enclosing all mapped occurrences of
the species. AOO and EOO and were calculated using ArcGIS software and
based on cleaned spatial datasets from the same dataset considered here.
AOO and EOO were provided as a range to accommodate the uncertainty
around several sites at which local extinction has been documented.

Differences between the published assessment and our calculations are
expected. We tried to use the same data sources and clean the data
following similar principles, but we did not use the exact same dataset,
and we did not consult experts in the steps of data preparation, so we
might be using a different subset of the available data.

Our results estimated a wider range of AOO values (152 — 196 ${km}^{2}$)
but slightly lower EOO values (1,414 — 1,566 ${km}^{2}$). These
differences are well within the thresholds of the category, so they
wouldn’t affect the outcome of the assessment.
