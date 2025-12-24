# Prune River Meter

Removes river meters above rm values in y.

## Usage

``` r
fwa_prune_rms(x, y)
```

## Arguments

- x:

  A data frame with integer columns blk, rm, parent_blk and parent_rm.

- y:

  A data frame with integer columns blk and rm.

## Value

A tibble of x with rows above the rms in y removed.
