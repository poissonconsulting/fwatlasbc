# Add Parent to Streams

Adds the parent blue line key (`parent_blk`) and the river meter on the
parent where the stream joins it (`parent_rm`) to stream linestrings
together with the distance (m) from the mouth to the parent
(`distance_to_parent`). The parent is the nearest other stream to the
mouth of each stream. If the mouth is more than `gap` m from the nearest
other stream then `parent_blk`, `parent_rm` and `distance_to_parent` are
`NA`.

## Usage

``` r
fwa_add_parent_to_streams(
  x,
  gap = 1,
  elevation = FALSE,
  source = NULL,
  zoom = 12
)
```

## Arguments

- x:

  An sf tibble with a column blk and linestrings of streams (or stream
  segments).

- gap:

  A positive real number specifying the maximum gap (m) between the
  mouth of stream and its parent stream to be considered connected.

- elevation:

  A flag specifying whether to use the elevation to determine stream
  direction (or use the direction of the provided linestrings)

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

A copy of `x` with integer column `parent_blk` and numeric columns
`parent_rm` and `distance_to_parent`.

## Details

The stream segments for each blk are first joined using
[`fwa_join_stream_segments()`](https://poissonconsulting.github.io/fwatlasbc/reference/fwa_join_stream_segments.md).
The start of each joined stream is taken to be its mouth and river
meters on the parent are measured from its start. Unless
`elevation = TRUE`, streams digitized from source to mouth must first be
reversed using
[`fwa_reverse_streams()`](https://poissonconsulting.github.io/fwatlasbc/reference/fwa_reverse_streams.md).
The geometry of `x` is not altered.

## See also

[`fwa_convert_streams_to_rms()`](https://poissonconsulting.github.io/fwatlasbc/reference/fwa_convert_streams_to_rms.md)

## Examples

``` r
if (FALSE) { # \dontrun{
watershed <- fwa_add_watershed_to_blk(data.frame(blk = 356308001, rm = 1000))
network <- fwa_add_collection_to_polygon(watershed)
network <- select(network, blk = blue_line_key)
fwa_add_parent_to_streams(network)
} # }
```
