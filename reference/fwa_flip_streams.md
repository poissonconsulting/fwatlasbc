# Flip Streams

Ensures that stream linestrings are oriented in an upstream direction,
i.e., that the start (mouth) of each linestring is lower than its end.
The elevations of the start and end of each linestring are obtained
using
[`fwa_add_elevatr_elevation_to_point()`](https://poissonconsulting.github.io/fwatlasbc/reference/fwa_add_elevatr_elevation_to_point.md)
or
[`fwa_add_gm_elevation_to_point()`](https://poissonconsulting.github.io/fwatlasbc/reference/fwa_add_gm_elevation_to_point.md)
and linestrings whose start is more than `min_difference` m higher than
their end are reversed. All other linestrings are not altered.

## Usage

``` r
fwa_flip_streams(x, source = NULL, zoom = 12, min_difference = 5)
```

## Arguments

- x:

  An sf tibble with a column blk and linestrings of streams (or stream
  segments).

- source:

  A string of the source of the elevations. Possible values are
  `"elevatr"` (no API key required) and `"google"` (requires a Google
  Maps Elevation API key). If `NULL` (the default) `"google"` is used
  but this is **\[deprecated\]** and `source` should be set explicitly.

- zoom:

  A whole number between 1 and 14 of the zoom level of the Terrain
  Tiles. Higher zoom levels have a finer resolution but take longer to
  download. Ignored if `source = "google"`.

- min_difference:

  A non-negative number of the minimum difference (m) between the
  elevations of the start and end of a linestring for it to be reversed.

## Value

A copy of `x` with the linestrings oriented upstream.

## See also

[`fwa_add_elevatr_elevation_to_point()`](https://poissonconsulting.github.io/fwatlasbc/reference/fwa_add_elevatr_elevation_to_point.md),
[`fwa_add_gm_elevation_to_point()`](https://poissonconsulting.github.io/fwatlasbc/reference/fwa_add_gm_elevation_to_point.md)
and
[`fwa_join_stream_segments()`](https://poissonconsulting.github.io/fwatlasbc/reference/fwa_join_stream_segments.md)

## Examples

``` r
if (FALSE) { # \dontrun{
watershed <- fwa_add_watershed_to_blk(data.frame(blk = 356308001, rm = 1000))
network <- fwa_add_collection_to_polygon(watershed)
network <- select(network, blk = blue_line_key)
fwa_flip_streams(network)
} # }
```
