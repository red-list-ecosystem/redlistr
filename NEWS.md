# redlistr 1.0.4
* Submission of update to CRAN to remove dependencies on retiring packages.
* Updated functions to remove dependencies on rgdal and rgeos.

# redlistr 1.0.3.9000
* Updated functions to be generic for easier future implementation of different
spatial formats as inputs.
* Added new functionality to makeAOOGrid to allow Spatial Polygons as input.
* Added additional tests for AOO functions.

# redlistr 1.0.3
* Submission of update to CRAN to ensure compatibility with raster.

# redlistr 1.0.2.9002
* Updated gridUncertainty functions to ensure compatibility with the parameter
requirements for raster::shift().

# redlistr 1.0.2.9001
* Fixed gridUncertaintySimulation.

# redlistr 1.0.2.9000
* Changed the way the random jittering in gridUncertainty works to be more
useful as the splits get smaller.

# redlistr 1.0.2
* Submission of update to CRAN with improvements from reviewers.

# redlistr 1.0.1.9002
* Fixed ARC calculation in extrapolateEstimate.
* Added tests to ensure extrapolateEstimate calculations are correct.
* Renamed ARC to Annual Rate of Change
* Fixed getArea when raster cells are not squares

# redlistr 1.0.1.9001
* Added sequentialExtrapolate to return sequence of estimates calculated by the
selected algorithms.
* Updated extrapolateEstimate output wording.

# redlistr 1.0.1.9000
* Added extrapolateEstimate which will replace futureAreaEstimate.

# redlistr 1.0.1
* Fixed getArea to output a single value and included test
* Submission of update to CRAN

# redlistr 1.0.0.9004
* Added test for SpatialPolygons for getArea
* Updated vignette to have single date

# redlistr 1.0.0.9003
* Fixed an error in getArea where a raster with a single class wouldn't be
calculated properly
* Updated tests to reflect changes to getDeclineStats

# redlistr 1.0.0.9002
* Changed getDeclineStats so ARC now returns a percentage
* Updated futureAreaEstimate to accept ARC as a percentage

# redlistr 1.0.0.9001
* Updated all functions to accept SpatialPoints and SpatialPointsDataFrame as
input.

# redlistr 1.0.0.9000
* Cleaned up code in getEOOPolygon.
* gridUncertainty functions now have default min.percent.rule set to FALSE.
* gridUncertainty now uses while, eliminating need for max.rounds parameter.
* gridUncertaintyBase now includes two new parameters: restriction and
min.grids.shift.
* Added gridUncertaintyRestricted.

# redlistr 1.0.0
* redlistr is now on CRAN!
