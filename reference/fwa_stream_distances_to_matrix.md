# Stream Distances to Matrix

Calculates the distances along the stream network between stream
distances (river meters). The distance is `Inf` if the river meters are
on streams that are not connected.

## Usage

``` r
fwa_stream_distances_to_matrix(x, streams, id = NULL)
```

## Arguments

- x:

  A data frame (including sf objects) with whole numeric column blk and
  numeric column rm of the river meters. All blk values must be in
  `streams$blk` and all rm values must be non-negative.

- streams:

  A data frame with whole numeric column blk, whole numeric column
  parent_blk and numeric column parent_rm. The stream network must not
  be circular.

- id:

  A string of the name of a column in `x` with unique non-missing values
  that are converted to character and used as the row and column names
  of the matrix or `NULL`.

## Value

A numeric matrix of the distances in m with one row and one column for
each row in `x`.

## Details

The stream network is defined by the parent blue line key (`parent_blk`)
and the river meter on the parent where each stream joins it
(`parent_rm`) as returned by
[`fwa_add_parent_to_streams()`](https://poissonconsulting.github.io/fwatlasbc/reference/fwa_add_parent_to_streams.md).
Parents that are not in `streams$blk` are treated as root streams.

## See also

[`fwa_add_parent_to_streams()`](https://poissonconsulting.github.io/fwatlasbc/reference/fwa_add_parent_to_streams.md)

## Examples

``` r
streams <- data.frame(
  blk = c(1, 2, 3),
  parent_blk = c(NA, 1, NA),
  parent_rm = c(NA, 400, NA)
)
rms <- data.frame(
  site = c("a", "b", "c", "d"),
  blk = c(1, 1, 2, 3),
  rm = c(100, 500, 50, 10)
)
fwa_stream_distances_to_matrix(rms, streams)
#>      [,1] [,2] [,3] [,4]
#> [1,]    0  400  350  Inf
#> [2,]  400    0  150  Inf
#> [3,]  350  150    0  Inf
#> [4,]  Inf  Inf  Inf    0
fwa_stream_distances_to_matrix(rms, streams, id = "site")
#>     a   b   c   d
#> a   0 400 350 Inf
#> b 400   0 150 Inf
#> c 350 150   0 Inf
#> d Inf Inf Inf   0
```
