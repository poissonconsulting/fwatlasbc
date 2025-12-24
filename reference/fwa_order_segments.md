# Get Stream Segments in Order

Orders stream segments so they are in order to be stitched together.

## Usage

``` r
fwa_order_segments(x)
```

## Arguments

- x:

  A spatial data frame.

## Value

A spatial data frame.

## Details

The function works by locating the longest segment and then finding the
nearest segment to it and continues this pattern until all segments have
been iterated through.

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

 fwa_order_segments(df)
} # }
```
