# Reverse Streams

Reverses the direction of the linestrings of the streams with the
specified blue line keys. All other linestrings are not altered.

## Usage

``` r
fwa_reverse_streams(x, blk)
```

## Arguments

- x:

  An sf tibble with a column blk and linestrings of streams (or stream
  segments).

- blk:

  A whole numeric vector of the unique blue line keys of the streams to
  reverse. All values must be in `x$blk`.

## Value

A copy of `x` with the linestrings of the specified streams reversed.

## See also

[`fwa_flip_streams()`](https://poissonconsulting.github.io/fwatlasbc/reference/fwa_flip_streams.md)
and
[`fwa_mapview_streams()`](https://poissonconsulting.github.io/fwatlasbc/reference/fwa_mapview_streams.md)

## Examples

``` r
if (FALSE) { # \dontrun{
watershed <- fwa_add_watershed_to_blk(data.frame(blk = 356308001, rm = 1000))
network <- fwa_add_collection_to_polygon(watershed)
network <- select(network, blk = blue_line_key)
fwa_reverse_streams(network, 356308001)
} # }
```
