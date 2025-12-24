# Snap Stream Measure to Point

Assigns closest stream measure in m to each spatial point. If the blue
line key (blk) is missing then it is also assigned together with the
distance to the stream (distance_to_stream) in m.

## Usage

``` r
fwa_snap_stream_measure_to_point(x, streams, ...)
```

## Arguments

- x:

  An sf object of spatial points with optional integer column blk.

- streams:

  An sf object of spatial linestrings with blk column.

- ...:

  Additional columns to group by when assigning.

## Value

An updated version of x with integer columns blk and stream_measure and
numeric column distance_to_stream.

## See also

[`fwa_snap_rm_to_point()`](https://poissonconsulting.github.io/fwatlasbc/reference/fwa_snap_rm_to_point.md)

## Examples

``` r
if (FALSE) { # \dontrun{
watershed <- fwa_add_watershed_to_blk(data.frame(blk = 356308001, rm = 1000))
network <- fwa_add_collection_to_polygon(watershed)
network$blk <- network$blue_line_key
streams <- fwa_join_stream_segments(network)
points <- fwa_add_rms_to_blk(data.frame(blk = 356308001))
fwa_snap_stream_measure_to_point(points, streams)
} # }
```
