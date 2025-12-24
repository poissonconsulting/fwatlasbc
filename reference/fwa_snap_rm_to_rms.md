# Snap River Meter to River Meters

Assigns closest river meter to river meters based on blue line keys. If
x already includes new_rm column then non-missing values are preserved.
The non-missing new_rm values must be ordered (with respect to x\$rm)
and must be present in rm\$rm. If x already includes new_blk then river
meters can be assigned to a creek with a different blue line key and/or
river meters from multiple creeks can be assigned to the same creek (for
example in the case of a previously unmapped side channel and the
mainstem).

## Usage

``` r
fwa_snap_rm_to_rms(x, rm, snap_mouths = FALSE)
```

## Arguments

- x:

  An sf object of spatial points with blk and rm columns and optional
  new_rm integer and new_blk columns.

- rm:

  An sf object of spatial point with blk and rm columns.

- snap_mouths:

  A flag specifying whether to snap pairs of streams at their mouths (rm
  = 0) where new_rm is not already set.

## Value

An updated version of x with integer columns blk, rm, new_blk, new_rm
and numeric column distance_to_new_rm.

## Details

The closest river meter is snapped to each rm (by blk = new_blk) and
missing new_rm values are replaced with the corresponding rm value. The
new_rm values are then ordered by adjusting the values so that firstly
all previous values are not greater than each provided new_rm value and
then all subsequent values are not less than the maximum previous value.
Next all runs of two or more identical new_rm values that do not include
a provided new_rm are interpolated between the previous and subsequent
new_rm values based on the original rm spacing and then snapped to the
closest rm value in rm.

To ensure that pairs of streams snap at their mouths set the new_rm to
be 0 where the rm is 0 or set `snap_mouths = TRUE`

## Examples

``` r
rm <- fwa_add_rms_to_blk(data.frame(blk = 356308001))
x <- rm[rm$rm %in% c(0, 2000, 5000, 6000, 7000), ]
rm <- rm[rm$rm %in% c(1000, 3000, 4000, 8000, 9000, 10000), ]
fwa_snap_rm_to_rms(x, rm)
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
```
