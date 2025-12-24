# Join Stream Segments

Converts a tibble of stream segment linestrings to stream linestrings.

## Usage

``` r
fwa_join_stream_segments(x, elevation = FALSE, reverse = integer())
```

## Arguments

- x:

  An sf tibble with a column blk and linestrings of stream segments.

- elevation:

  A flag specifying whether to use the elevation from Google Maps to
  determine stream direction (or use the direction of the provided
  linestrings)

- reverse:

  A whole numeric vector of streams to reverse direction ignoring
  elevation.

## Value

An sf tibble with the columns blk and sfc column point geometry.

## See also

[`fwa_convert_streams_to_rms()`](https://poissonconsulting.github.io/fwatlasbc/reference/fwa_convert_streams_to_rms.md)

## Examples

``` r
if (FALSE) { # \dontrun{
watershed <- fwa_add_watershed_to_blk(data.frame(blk = 356308001, rm = 1000))
network <- fwa_add_collection_to_polygon(watershed)
network <- select(network, blk = blue_line_key)
fwa_join_stream_segments(network)
} # }
```
