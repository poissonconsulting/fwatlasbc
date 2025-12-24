# Snap River Meter to Point

Assigns closest river meter to each spatial point. If the blue line key
(blk) is missing then it is also assigned together with the distance to
the river meter (distance_to_rm) in m.

## Usage

``` r
fwa_snap_rm_to_point(x, rm, ...)
```

## Arguments

- x:

  An sf object of spatial points with optional integer column blk.

- rm:

  An sf object of spatial point with blk and rm columns.

- ...:

  Additional columns to group by when assigning. Elements with missing
  values in rm are assigned to any value in x.

## Value

An updated version of x with integer columns blk and rm and numeric
column distance_to_rm.

## Examples

``` r
rm <- fwa_add_rms_to_blk(data.frame(blk = 356308001))
x <- rm[rm$rm %in% c(0, 2000, 5000, 6000, 7000), ]
rm <- rm[rm$rm %in% c(1000, 3000, 4000, 8000, 9000, 10000), ]
fwa_snap_rm_to_point(x, rm)
#> Simple feature collection with 5 features and 4 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 1290725 ymin: 467231.2 xmax: 1294079 ymax: 469483.5
#> Projected CRS: NAD83 / BC Albers
#> # A tibble: 5 × 5
#>         blk    rm distance_to_rm elevation           geometry
#>       <int> <int>          <dbl>     <dbl>        <POINT [m]>
#> 1 356308001  1000           874.      9.63 (1290725 469483.5)
#> 2 356308001  3000           536.      7.5  (1292439 468922.9)
#> 3 356308001  4000           754.      9.78 (1293158 467231.2)
#> 4 356308001  4000           611.      9.78 (1293794 467668.3)
#> 5 356308001  8000           515.      9.78 (1294079 467938.4)
```
