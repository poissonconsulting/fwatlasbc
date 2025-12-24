# Add Watershed to Blue Line Key

Adds polygon (geometry) of aggregated fundamental watersheds to blue
line key (blk). The rm distances which is in meters is from the river
mouth.

## Usage

``` r
fwa_add_watershed_to_blk(
  x,
  exclude = FALSE,
  epsg = getOption("fwa.epsg", 3005),
  nocache = getOption("fwa.nocache", FALSE)
)
```

## Arguments

- x:

  An sf object with a polygon sfc column specifying watersheds and an
  optional rm column specfying the river meter. The rm is set to be 0 if
  missing.

- exclude:

  A logical vector specifying whether to exclude the fundamental
  watershed in which the start falls.

- epsg:

  A positive whole number of the epsg to transform features to.

- nocache:

  A flag specifying whether or not to cache results.

## Value

An sf tibble with the columns of x plus sf column geometry.

A sf object

## See also

[`fwapgr::fwa_watershed_at_measure()`](https://rdrr.io/pkg/fwapgr/man/fwa_watershed_at_measure.html).

## Examples

``` r
if (FALSE) { # \dontrun{
fwa_add_watershed_to_blk(data.frame(blk = 356308001))
} # }
```
