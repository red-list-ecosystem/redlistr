# Changelog

## redlistr 2.0.0

- Create SpatRaster and sf methods for getAOO
- Deprecate raster and sp usage
- Replace uncertainty functions with jitter functionality to makeAOOGrid
- Update namespace, dependencies, and function documentation
- Add classes, and summary, print and plot functions
- Modify functions to accept multiple ecosystems at once
- Update vignettes

## redlistr 1.0.4

CRAN release: 2023-10-03

- Submission of update to CRAN to remove dependencies on retiring
  packages.
- Updated functions to remove dependencies on rgdal and rgeos.

## redlistr 1.0.3.9000

- Updated functions to be generic for easier future implementation of
  different spatial formats as inputs.
- Added new functionality to makeAOOGrid to allow Spatial Polygons as
  input.
- Added additional tests for AOO functions.

## redlistr 1.0.3

CRAN release: 2019-07-11

- Submission of update to CRAN to ensure compatibility with raster.

## redlistr 1.0.2.9002

- Updated gridUncertainty functions to ensure compatibility with the
  parameter requirements for raster::shift().

## redlistr 1.0.2.9001

- Fixed gridUncertaintySimulation.

## redlistr 1.0.2.9000

- Changed the way the random jittering in gridUncertainty works to be
  more useful as the splits get smaller.

## redlistr 1.0.2

CRAN release: 2018-11-02

- Submission of update to CRAN with improvements from reviewers.

## redlistr 1.0.1.9002

- Fixed ARC calculation in extrapolateEstimate.
- Added tests to ensure extrapolateEstimate calculations are correct.
- Renamed ARC to Annual Rate of Change
- Fixed getArea when raster cells are not squares

## redlistr 1.0.1.9001

- Added sequentialExtrapolate to return sequence of estimates calculated
  by the selected algorithms.
- Updated extrapolateEstimate output wording.

## redlistr 1.0.1.9000

- Added extrapolateEstimate which will replace futureAreaEstimate.

## redlistr 1.0.1

CRAN release: 2018-05-11

- Fixed getArea to output a single value and included test
- Submission of update to CRAN

## redlistr 1.0.0.9004

- Added test for SpatialPolygons for getArea
- Updated vignette to have single date

## redlistr 1.0.0.9003

- Fixed an error in getArea where a raster with a single class wouldn’t
  be calculated properly
- Updated tests to reflect changes to getDeclineStats

## redlistr 1.0.0.9002

- Changed getDeclineStats so ARC now returns a percentage
- Updated futureAreaEstimate to accept ARC as a percentage

## redlistr 1.0.0.9001

- Updated all functions to accept SpatialPoints and
  SpatialPointsDataFrame as input.

## redlistr 1.0.0.9000

- Cleaned up code in getEOOPolygon.
- gridUncertainty functions now have default min.percent.rule set to
  FALSE.
- gridUncertainty now uses while, eliminating need for max.rounds
  parameter.
- gridUncertaintyBase now includes two new parameters: restriction and
  min.grids.shift.
- Added gridUncertaintyRestricted.

## redlistr 1.0.0

CRAN release: 2017-07-07

- redlistr is now on CRAN!
