# Add Blue Line Key to Longitude and Latitude

Adds integer blue line key (blk) and numeric river meter (rm) column and
sfc point (geometry) column of the closest point on the stream network
(by default within 5 km) to the point specified by the lon and lat
(WGS84).

## Usage

``` r
fwa_add_blk_to_lon_lat(
  x,
  tolerance = 5000,
  limit = 1,
  epsg = getOption("fwa.epsg", 3005),
  nocache = getOption("fwa.nocache", FALSE)
)
```

## Arguments

- x:

  A data frame with numeric longitude (long) and latitude (lat) columns.

- tolerance:

  A number of the tolerance in m.

- limit:

  A positive whole number indicating the maximum number of features to
  return.

- epsg:

  A positive whole number of the EPSG projection for the geometry.

- nocache:

  A flag specifying whether or not to cache results.

## Value

An sf tibble with the columns of x plus integer column blk, real columns
rm and distance_to_lon_lat and sfc point column geometry

## Details

If a match isn't found the row is dropped.

## Examples

``` r
if (FALSE) { # \dontrun{
fwa_add_blk_to_lon_lat(data.frame(lon = -132.26, lat = 53.36))
} # }
```
