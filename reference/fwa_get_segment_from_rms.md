# Get Segment from River Meter

Gets sf tibble of section blk, rm_start, rm_end, segment and line
geometry.

## Usage

``` r
fwa_get_segment_from_rms(x, segment = "segment")
```

## Arguments

- x:

  A data frame with integer columns blk and rm.

- segment:

  A string of the name of the column in x, which must be a character or
  factor vector, that specifies which segment each rms belongs to.

## Value

A sf tibble with integer columns blk, rm_start, rm_end, character/factor
segment and a line geometry.
