# Add River Meters to Blue Line Key

Adds distances (rm) and spatial coordinates (geometry) of regularly
spaced points along blue line key (blk). All distances which are in
meters are from the river mouth.

## Usage

``` r
fwa_add_rms_to_blk(
  x,
  interval = 1000,
  start = 0,
  end = Inf,
  epsg = getOption("fwa.epsg", 3005),
  nocache = getOption("fwa.nocache", FALSE)
)
```

## Arguments

- x:

  A data frame with integer column blk.

- interval:

  A whole numeric of the distance between points.

- start:

  A whole numeric of the start distance.

- end:

  An integer of the end distance.

- epsg:

  A positive whole number of EPSG projection for the coordinates.

- nocache:

  A flag specifying whether or not to cache results.

## Value

An sf tibble with the columns of x plus integer column rm and sf column
geometry.

## Examples

``` r
if (FALSE) { # \dontrun{
fwa_add_rms_to_blk(data.frame(blk = 356308001))
} # }
```
