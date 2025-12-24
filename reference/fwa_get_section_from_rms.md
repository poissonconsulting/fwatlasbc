# Get Section from River Meter

Gets sf tibble of section blk, rm, length and geometry where sections
are defined by the their most upstream point.

## Usage

``` r
fwa_get_section_from_rms(x, section = "section")
```

## Arguments

- x:

  A data frame with integer columns blk and rm.

- section:

  A string of the name of the column in x that specifies the sections.

## Value

A sf tibble with integer columns blk, rm, length and a geometry where rm
and geometry are the upstream end of the section.
