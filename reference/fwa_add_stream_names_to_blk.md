# Add Blue Line Key(s) to Stream Name

Adds blue line keys (blk) to stream names. There may be more than one
stream with the same name.

## Usage

``` r
fwa_add_stream_names_to_blk(x, stream_name = fwatlasbc::fwa_stream_name)
```

## Arguments

- x:

  A data frame with whole numeric column blk.

- stream_name:

  A data frame with whole numeric column blk and character column
  stream_name.

## Value

A tibble with the columns of x plus an integer column blk.

## Examples

``` r
fwa_add_stream_names_to_blk(data.frame(blk = 360886335L))
#> # A tibble: 1 × 2
#>         blk stream_name     
#>       <int> <chr>           
#> 1 360886335 Aaltanhash River
```
