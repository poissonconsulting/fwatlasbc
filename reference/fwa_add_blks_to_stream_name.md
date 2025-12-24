# Add Blue Line Key(s) to Stream Name

Adds blue line keys (blk) to stream names. There may be more than one
stream with the same name.

## Usage

``` r
fwa_add_blks_to_stream_name(x, stream_name = fwatlasbc::fwa_stream_name)
```

## Arguments

- x:

  A data frame with character column stream_name.

- stream_name:

  A data frame with whole numeric column blk and character column
  stream_name.

## Value

A tibble with the columns of x plus an integer column blk.

## Examples

``` r
fwa_add_blks_to_stream_name(data.frame(stream_name = "Sangan River"))
#> # A tibble: 1 × 2
#>   stream_name        blk
#>   <chr>            <int>
#> 1 Sangan River 360879896
```
