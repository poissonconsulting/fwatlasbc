# Add New Blue Line Key and River Meter to Blue Line Key and River Meter

Adds new blk and rm values to existing blk and rm values based on look
up in second data frame.

## Usage

``` r
fwa_add_new_blk_rm_to_blk_rm(
  x,
  y,
  blk = "blk",
  rm = "rm",
  blk2 = "blk",
  rm2 = "rm",
  new_blk = "new_blk",
  new_rm = "new_rm",
  new_blk_to = new_blk,
  new_rm_to = new_rm
)
```

## Arguments

- x:

  A tibble (or sf object) with blk and rm columns.

- y:

  A tibble (or sf object) with unique blk and rm columns and new blk
  (optional) and rm columns.

- blk:

  A string of the name of the blk column in x.

- rm:

  A string of the name of the rm column in x.

- blk2:

  A string of the name of the blk column in y.

- rm2:

  A string of the name of the rm column in y.

- new_blk:

  An optional string of the name of the new blk column in y. If NULL it
  is not used in the join.

- new_rm:

  A string of the name of the new rm column in y.

- new_blk_to:

  A string specifying the name to rename the new blk column with.

- new_rm_to:

  A string specifying the name to rename the new blk column with.

## Value

An updated version of x with additional columns for the new blk and rm
columns.

## See also

`[fwa_add_new_rm_to_blk_rm()]`
