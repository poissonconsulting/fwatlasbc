# Map View River Meters

Map View River Meters

## Usage

``` r
fwa_mapview_rms(
  x,
  layer = NULL,
  zcol = "rm",
  legend = FALSE,
  npoint = 250,
  ...
)
```

## Arguments

- x:

  An sf data frame with unique integer columns blk and rm.

- layer:

  A string of the column to layer the points by.

- zcol:

  A string of the column to color points by.

- legend:

  A flag specifying whether to plot a legend.

- npoint:

  An indication of the total number of points to plot.

- ...:

  Additional arguments passed to
  [`mapview::mapview()`](https://r-spatial.github.io/mapview/reference/mapView.html).
