# Stitch Segments

Adds segments between the disconnected parts of a MULTILINESTRING.

## Usage

``` r
fwa_stitch_segments(x, ..., tolerance = 5)
```

## Arguments

- x:

  A spatial data frame.

- ...:

  Unused.

- tolerance:

  A numeric value for the maximum euclidean distance between the end a
  segment to the start of a segment that will create a stitched segment.
  If the distance is greater then the tolerance then a segment will not
  be created.

## Value

A data frame with the stitched segments added into the geometries.

## Details

The `fwa_stitch_segments()` assumes that the segments within the
MULTILINESTRING are in order and the correct direction to be stitched
together.

If the segments could not be joined they will be returned as a
MULTILINESTRING, if the segments have no gaps left the row will be
returned as a LINESTRING.

## Examples

``` r
if (FALSE) { # \dontrun{
test_stream <- sf::st_sfc(
  sf::st_multilinestring(
    c(sf::st_linestring(matrix(c(0, 0, 1, 1), ncol = 2, byrow = TRUE)),
      sf::st_linestring(matrix(c(1.05, 1.05, 2, 2), ncol = 2, byrow = TRUE)),
      sf::st_linestring(matrix(c(2.1, 2.1, 3, 3), ncol = 2, byrow = TRUE))
    )
  ),
  crs = 26911
 )

 df <- data.frame(
  name = "stream 1",
  blk = 1,
  geometry = test_stream
 ) |>
 sf::st_set_geometry("geometry")

 # to create two 2 segments to connect the whole line
 fwa_stitch_segments(df)

 # tolerance is set so only 1 segment is added and the line is still not connected
 fwa_stitch_segments(df, tolerance = 0.1)
} # }
```
