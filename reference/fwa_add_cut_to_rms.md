# Add Cut to River Meter

Adds value(s) in cut column in y to to column of same name in x based on
blk and rm_start and rm_end in y.

## Usage

``` r
fwa_add_cut_to_rms(x, y, cut = "cut")
```

## Arguments

- x:

  A data frame with integer columns blk and rm.

- y:

  A data frame with integer columns blk, rm_start, rm_end and column
  specified in cut.

- cut:

  A string of the name of the column in y with the values.

## Value

A tibble of x with cut column from y.
