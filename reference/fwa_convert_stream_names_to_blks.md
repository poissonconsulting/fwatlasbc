# Converts Stream Names to Blue Line Keys

Each stream name is converted to a blue line key by calculating it's
integer hash.

## Usage

``` r
fwa_convert_stream_names_to_blks(names)
```

## Arguments

- names:

  A character vector of stream names.

## Value

An positive integer vector of blue line keys.

## Details

This function is only expected to be used when a blue line key does not
already exist.

## Examples

``` r
fwa_convert_stream_names_to_blks(c("a stream name", "a stream name2"))
#> [1] 1172914356  495612387
```
