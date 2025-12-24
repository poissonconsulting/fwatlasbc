# Convert Stream Network to River Meters

Converts a tibble of a BC stream network to river meters.

## Usage

``` r
fwa_convert_stream_network_to_rms(x, interval = 5, tolerance = 0.1)
```

## Arguments

- x:

  An sf tibble of a stream network.

- interval:

  A whole number of the distance between points.

- tolerance:

  A number of the acceptable discrepancy in meters in the network
  lengths.

## Value

An sf tibble with the columns of x plus integer column rm and sf column
geometry.

## See also

[`fwa_convert_streams_to_rms()`](https://poissonconsulting.github.io/fwatlasbc/reference/fwa_convert_streams_to_rms.md)

## Examples

``` r
if (FALSE) { # \dontrun{
watershed <- fwa_add_watershed_to_blk(data.frame(blk = 356308001, rm = 1000))
network <- fwa_add_collection_to_polygon(watershed)
fwa_convert_stream_network_to_rms(network, interval = 100)
} # }
```
