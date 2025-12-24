# Swap Branches of River Meters

Swaps two branches of river meters.

## Usage

``` r
fwa_swap_branches_rms(x, y, adjust_points = TRUE)
```

## Arguments

- x:

  A data frame with integer columns blk, rm, parent_blk and parent_rm.

- y:

  A data frame with integer column blk specifying the blue line key that
  currently begins at the confluence of the two branches.

- adjust_points:

  A flag specifying whether to adjust the coordinates of points which
  move.

## Value

A copy of x with the branches swapped.
