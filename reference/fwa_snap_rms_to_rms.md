# Snap River Meters to River Meters

Assigns closest river meters to river meters by blue line keys using
[`fwa_snap_rm_to_rms()`](https://poissonconsulting.github.io/fwatlasbc/reference/fwa_snap_rm_to_rms.md)
rm must not have an existing new_rm column.

## Usage

``` r
fwa_snap_rms_to_rms(x, rm, snap_mouths = FALSE)
```

## Arguments

- x:

  An sf object of spatial points with blk and rm columns and optional
  new_blk and new_rm integer column.

- rm:

  An sf object of spatial point with blk and rm columns.

- snap_mouths:

  A flag specifying whether to snap pairs of streams at their mouths (rm
  = 0) where new_rm is not already set.

## Value

A named list with an updated versions of x and rm with integer columns
blk, new_blk, rm and new_rm and numeric column distance_to_new_rm.

## Details

x is first snapped to rm then rm is snapped to x while ensuring that the
links between x and rm are bidirectional as much as possible.

## See also

[`fwa_snap_rm_to_rms()`](https://poissonconsulting.github.io/fwatlasbc/reference/fwa_snap_rm_to_rms.md)

## Examples

``` r
rm <- fwa_add_rms_to_blk(data.frame(blk = 356308001))
x <- rm[rm$rm %in% c(0, 2000, 5000, 6000, 7000), ]
rm <- rm[rm$rm %in% c(1000, 3000, 4000, 8000, 9000, 10000), ]
fwa_snap_rms_to_rms(x, rm)
#> $x
#> Simple feature collection with 5 features and 6 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 1290725 ymin: 467231.2 xmax: 1294079 ymax: 469483.5
#> Projected CRS: NAD83 / BC Albers
#> # A tibble: 5 × 7
#>         blk   new_blk    rm new_rm distance_to_new_rm elevation
#>       <int>     <int> <int>  <int>              <dbl>     <dbl>
#> 1 356308001 356308001     0   1000               874.      9.63
#> 2 356308001 356308001  2000   3000               536.      7.5 
#> 3 356308001 356308001  5000   4000               754.      9.78
#> 4 356308001 356308001  6000   8000               905.      9.78
#> 5 356308001 356308001  7000   8000               515.      9.78
#> # ℹ 1 more variable: geometry <POINT [m]>
#> 
#> $rm
#> Simple feature collection with 6 features and 6 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 1291542 ymin: 467912.1 xmax: 1295576 ymax: 469173.9
#> Projected CRS: NAD83 / BC Albers
#> # A tibble: 6 × 7
#>         blk   new_blk    rm new_rm distance_to_new_rm elevation
#>       <int>     <int> <int>  <int>              <dbl>     <dbl>
#> 1 356308001 356308001  1000      0               874.     10.1 
#> 2 356308001 356308001  3000   2000               536.      9.69
#> 3 356308001 356308001  4000   5000               754.      9.78
#> 4 356308001 356308001  8000   7000               515.     10   
#> 5 356308001 356308001  9000   7000               645.     10.5 
#> 6 356308001 356308001 10000   7000              1497.     10.5 
#> # ℹ 1 more variable: geometry <POINT [m]>
#> 
```
