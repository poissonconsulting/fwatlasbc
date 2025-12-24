# Map View River Meters

Maps two alternative stream networks by adding links from each point in
x to matching point in y.

## Usage

``` r
fwa_mapview_rms_to_rms(x, y, zcol = "rm", npoint = 250)
```

## Arguments

- x:

  An sf data frame with unique integer columns blk and rm and integer
  column new_rm.

- y:

  An sf data frame with unique integer columns blk and rm.

- zcol:

  A string of the column to color points by.

- npoint:

  An indication of the total number of points to plot.
