# Add Google Maps Elevation to Point

Add Google Maps Elevation to Point

## Usage

``` r
fwa_add_gm_elevation_to_point(
  x,
  chunk_size = 300L,
  digits = 7,
  key = Sys.getenv("GOOGLE_MAPS_ELEVATION_API_KEY")
)
```

## Arguments

- x:

  An sf object of spatial points.

- chunk_size:

  The number of rows to include in each API query.

- digits:

  The number of digits to round the latitude and longitude by before
  querying the elevation from the API

- key:

  A string of the Google Maps Elevation API key.

## Value

An updated version of x with numeric column elevation.

## See also

[`googleway::google_elevation()`](https://rdrr.io/pkg/googleway/man/google_elevation.html)

## Examples

``` r
if (FALSE) { # \dontrun{
rm <- fwa_add_rms_to_blk(data.frame(blk = 356308001))
fwa_add_gm_elevation_to_point(rm)
} # }
```
