# Add Point to Stream Measure

Adds point geometry for stream measure in m based on the blue line key
(blk) and the proportion of the stream measure along the stream. If the
proportion is \>= 1 then the geometry is set to the top of the stream.

## Usage

``` r
fwa_add_point_to_stream_measure(x, streams, ...)
```

## Arguments

- x:

  A data frame with columns blk and stream_measure.

- streams:

  An sf object of spatial linestrings with blk column.

- ...:

  Additional columns to group by when assigning.

## Value

An updated version of x with numeric column proportion giving the
proportion of the stream measure along the stream and a geometry column.

## See also

[`fwa_snap_stream_measure_to_point()`](https://poissonconsulting.github.io/fwatlasbc/reference/fwa_snap_stream_measure_to_point.md)

## Examples

``` r
if (FALSE) { # \dontrun{
watershed <- fwa_add_watershed_to_blk(data.frame(blk = 356308001, rm = 1000))
network <- fwa_add_collection_to_polygon(watershed)
network$blk <- network$blue_line_key
streams <- fwa_join_stream_segments(network)
points <- fwa_add_rms_to_blk(data.frame(blk = 356308001))
points <- fwa_snap_stream_measure_to_point(points, streams)
points <- points[c("blk", "stream_measure")]
fwa_add_point_to_stream_measure(points, streams)
} # }
```
