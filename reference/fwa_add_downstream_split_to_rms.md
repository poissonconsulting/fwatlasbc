# Add Split to River Meter

Splits river meters with parent_blk and parent_rm columns into upstream
(TRUE) versus not (FALSE).

## Usage

``` r
fwa_add_downstream_split_to_rms(x, y)
```

## Arguments

- x:

  A data frame with integer columns blk, rm, parent_blk and parent_rm.

- y:

  A data frame with integer columns blk and rm and character column
  name.

## Value

A tibble of x with a logical column for each name in name.
