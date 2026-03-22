# trend class

A class to represent change in area over time for an ecosystem.

## Slots

- `input`:

  binary raster or list of sf POLYGON objects representing the input
  ecosystem extent

- `areas`:

  the area of the ecosystem in each layer or list element

- `model`:

  a fitted model object typically of the class lm containing the model
  used to calculate the trend

- `netdiff`:

  difference between the area in the first layer or list element and the
  last.

- `diff`:

  raster stack showing change over layers or list elements of the
  ecosystem extent.
