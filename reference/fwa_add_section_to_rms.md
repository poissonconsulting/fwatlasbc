# Add Section to River Meter

Adds section column in y to x based on blk and end rm in y. All rms in x
up to and including the end rm but before the previous end rm are
assigned the section column value (which can be missing).

## Usage

``` r
fwa_add_section_to_rms(x, y, section = "section")
```

## Arguments

- x:

  A data frame with integer columns blk and rm.

- y:

  A data frame with integer columns blk and rm and column specified in
  section.

- section:

  A string of the name of the column in y (can include missing values).

## Value

A tibble of x with section column from y.
