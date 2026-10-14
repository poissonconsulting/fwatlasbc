# Add elevatr Elevation to Point

Adds elevations from the Amazon Web Services Terrain Tiles using
[`elevatr::get_elev_point()`](https://rdrr.io/pkg/elevatr/man/get_elev_point.html).
Unlike
[`fwa_add_gm_elevation_to_point()`](https://poissonconsulting.github.io/fwatlasbc/reference/fwa_add_gm_elevation_to_point.md)
no API key is required.

## Usage

``` r
fwa_add_elevatr_elevation_to_point(x, zoom = 12)
```

## Arguments

- x:

  An sf object of spatial points.

- zoom:

  A whole number between 1 and 14 of the zoom level of the Terrain
  Tiles. Higher zoom levels have a finer resolution but take longer to
  download.

## Value

An updated version of x with numeric column elevation.

## Details

The connection timeout (s) and the number of attempts if the download
times out are set by the options `fwa.elevatr_timeout` (by default 10)
and `fwa.elevatr_tries` (by default 3). For example
`options(fwa.elevatr_timeout = 120, fwa.elevatr_tries = 5)`.

## See also

[`elevatr::get_elev_point()`](https://rdrr.io/pkg/elevatr/man/get_elev_point.html)
and
[`fwa_add_gm_elevation_to_point()`](https://poissonconsulting.github.io/fwatlasbc/reference/fwa_add_gm_elevation_to_point.md)

## Examples

``` r
if (FALSE) { # \dontrun{
rm <- fwa_add_rms_to_blk(data.frame(blk = 356308001))
fwa_add_elevatr_elevation_to_point(rm)
} # }
```
