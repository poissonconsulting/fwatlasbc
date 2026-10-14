# Join Stream Segments

Converts a tibble of stream segment linestrings to stream linestrings.

## Usage

``` r
fwa_join_stream_segments(
  x,
  elevation = FALSE,
  reverse = integer(),
  source = NULL,
  zoom = 12
)
```

## Arguments

- x:

  An sf tibble with a column blk and linestrings of stream segments.

- elevation:

  A flag specifying whether to use the elevation to determine stream
  direction (or use the direction of the provided linestrings)

- reverse:

  A whole numeric vector of streams to reverse direction ignoring
  elevation.

- source:

  A string of the source of the elevations if `elevation = TRUE`.
  Possible values are `"elevatr"` (no API key required) and `"google"`
  (requires a Google Maps Elevation API key). If `NULL` (the default)
  and `elevation = TRUE` `"google"` is used but this is
  **\[deprecated\]** and `source` should be set explicitly.

- zoom:

  A whole number between 1 and 14 of the zoom level of the Terrain
  Tiles. Higher zoom levels have a finer resolution but take longer to
  download. Ignored if `elevation = FALSE` or `source = "google"`.

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
