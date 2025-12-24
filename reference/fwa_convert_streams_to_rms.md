# Convert Streams to River Meters

Converts a tibble of streams to river meters. Unlike
[`fwa_convert_stream_network_to_rms()`](https://poissonconsulting.github.io/fwatlasbc/reference/fwa_convert_stream_network_to_rms.md)
it only requires the linestrings and the unique integer identifier for
each stream.

## Usage

``` r
fwa_convert_streams_to_rms(
  x,
  interval = 5,
  gap = 1,
  end = NULL,
  elevation = FALSE,
  reverse = integer()
)
```

## Arguments

- x:

  An sf tibble with a column blk and linestrings of streams.

- interval:

  A positive whole number of the distance (m) between points.

- gap:

  A positive real number specifying the maximum gap (m) between the
  mouth of stream and its parent stream to be considered connected.

- end:

  A positive whole number indicating how far (m) the end of the stream
  linestring has to be from the last interval to be included. To default
  `end = NULL` (equivalent to `end = interval + 1`) excludes ends.

- elevation:

  A flag specifying whether to use the elevation from Google Maps to
  determine stream direction (or use the direction of the provided
  linestrings)

- reverse:

  A whole numeric vector of streams to reverse direction ignoring
  elevation.

## Value

An sf tibble with the columns blk, integer column rm and sf column point
geometry.

## See also

[`fwa_convert_stream_network_to_rms()`](https://poissonconsulting.github.io/fwatlasbc/reference/fwa_convert_stream_network_to_rms.md)

## Examples

``` r
if (FALSE) { # \dontrun{
watershed <- fwa_add_watershed_to_blk(data.frame(blk = 356308001, rm = 1000))
network <- fwa_add_collection_to_polygon(watershed)
network <- select(network, blk = blue_line_key)
fwa_convert_streams_to_rms(network, interval = 100)
} # }
```
