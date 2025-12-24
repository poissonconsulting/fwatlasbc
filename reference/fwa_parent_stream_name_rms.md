# Parent Stream Name

Gets parent stream name.

## Usage

``` r
fwa_parent_stream_name_rms(x, rms, stream_name = fwatlasbc::fwa_stream_name)
```

## Arguments

- x:

  A character vector of one or more stream names.

- rms:

  A data frame with integer columns blk and parent_blk. There must be
  only one parent_blk for each blk.

- stream_name:

  A data frame with character column stream_name and integer column blk.
  There must be no more than one blk for the stream names specified.

## Value

A character vector of the parent blue line keys.
