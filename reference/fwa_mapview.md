# Map View

A wrapper on `mapview::mapview.sf()` that allows the user to layer by a
column and coerces hms columns to character (to avoid being dropped).

## Usage

``` r
fwa_mapview(x, layer = NULL, zcol = NULL, legend = FALSE, ...)
```

## Arguments

- x:

  An sf data frame.

- layer:

  A string of the column to layer the points by.

- zcol:

  A string of the column to color points by.

- legend:

  A flag specifying whether to plot a legend.

- ...:

  Additional arguments passed to
  [`mapview::mapview()`](https://r-spatial.github.io/mapview/reference/mapView.html).
