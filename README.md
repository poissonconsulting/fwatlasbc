
<!-- README.md is generated from README.Rmd. Please edit that file -->

# fwatlasbc <img src="man/figures/logo.png" align="right" />

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R build
status](https://github.com/poissonconsulting/fwatlasbc/workflows/R-CMD-check/badge.svg)](https://github.com/poissonconsulting/fwatlasbc/actions)
[![codecov](https://codecov.io/gh/poissonconsulting/fwatlasbc/branch/main/graph/badge.svg?token=x3TrvhuMbK)](https://codecov.io/gh/poissonconsulting/fwatlasbc)
[![License:
MIT](https://img.shields.io/badge/License-MIT-green.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

`fwatlasbc` is an R package for querying data from the [Freshwater
Atlas](https://www2.gov.bc.ca/gov/content/data/geographic-data-services/topographic-data/freshwater)
(FWA) of British Columbia. Spatial data are returned as [simple
features](https://github.com/r-spatial/sf).

`fwatlasbc` wraps
[`fwapgr`](https://github.com/poissonconsulting/fwapgr) an R package
that uses the [fwapg API](https://www.hillcrestgeo.ca/fwapg/index.html).

## Installation

You can install the latest version of `fwatlasbc` from
[GitHub](https://github.com/poissonconsulting/fwatlasbc) with:

``` r
# install.packages("devtools")
devtools::install_github("poissonconsulting/fwatlasbc")
```

## Demonstration

Find stream names using regular expression.

``` r
library(fwatlasbc)
streams <- fwa_find_stream_names("steep c")
streams
#> # A tibble: 2 × 1
#>   StreamName        
#>   <chr>             
#> 1 Steep Canyon Creek
#> 2 Steep Creek
```

Add blue line keys to stream names.

``` r
streams <- fwa_add_blks_to_stream_name(streams)
streams
#> # A tibble: 4 × 2
#>   StreamName               BLK
#>   <chr>                  <int>
#> 1 Steep Canyon Creek 360883036
#> 2 Steep Creek        356362258
#> 3 Steep Creek        356534225
#> 4 Steep Creek        356570155
streams <- streams[streams$BLK == 356534225,]
```

Get river meters (every 100 m).

``` r
rm <- fwa_add_rms_to_blk(streams, interval = 100)
rm
#> Simple feature collection with 46 features and 3 fields
#> Geometry type: POINT
#> Dimension:     XYZ
#> Bounding box:  xmin: 1657747 ymin: 728476.5 xmax: 1661313 ymax: 730795.9
#> z_range:       zmin: 1087 zmax: 2524.455
#> Projected CRS: NAD83 / BC Albers
#> # A tibble: 46 × 4
#>    StreamName        BLK    RM                      geometry
#>    <chr>           <int> <int>                   <POINT [m]>
#>  1 Steep Creek 356534225     0     Z (1657747 728476.5 1087)
#>  2 Steep Creek 356534225   100 Z (1657839 728506.7 1094.162)
#>  3 Steep Creek 356534225   200 Z (1657911 728572.2 1101.743)
#>  4 Steep Creek 356534225   300 Z (1657989 728633.1 1111.435)
#>  5 Steep Creek 356534225   400   Z (1658070 728691 1123.997)
#>  6 Steep Creek 356534225   500  Z (1658141 728754.7 1132.81)
#>  7 Steep Creek 356534225   600 Z (1658233 728792.8 1144.575)
#>  8 Steep Creek 356534225   700 Z (1658324 728830.6 1153.982)
#>  9 Steep Creek 356534225   800 Z (1658395 728897.5 1168.074)
#> 10 Steep Creek 356534225   900 Z (1658470 728960.1 1180.635)
#> # … with 36 more rows
```

Get watershed.

``` r
wshed <- fwa_add_watershed_to_blk(streams)
wshed
#> Simple feature collection with 1 feature and 2 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 1656218 ymin: 725423.1 xmax: 1661726 ymax: 732146.2
#> Projected CRS: NAD83 / BC Albers
#> # A tibble: 1 × 3
#>   StreamName        BLK                                                 geometry
#>   <chr>           <int>                                            <POLYGON [m]>
#> 1 Steep Creek 356534225 ((1658037 728924.8, 1658107 728964.9, 1658107 728964.9,…
```

Plot watershed and river meters.

``` r
ggplot2::ggplot() +
  ggplot2::geom_sf(data = wshed) +
  ggplot2::geom_sf(data = rm)
```

<img src="man/figures/README-unnamed-chunk-6-1.png" width="100%" />

## Inspiration

`fwatlasbc` supersedes the archived
[`fwabc`](https://github.com/poissonconsulting/fwabc) which retrieved
data via
[WFS](https://openmaps.gov.bc.ca/geo/pub/wfs?service=WFS&version=2.0.0&request=GetFeature&typeName=WHSE_BASEMAPPING.FWA_LAKES_POLY&outputFormat=json&SRSNAME=epsg%3A3005&CQL_FILTER=GNIS_NAME_1=%27Quamichan%20Lake%27)
using [`bcdata`](https://github.com/bcgov/bcdata).

## Creditation

`fwatlasbc` relies on [fwapg
API](https://www.hillcrestgeo.ca/fwapg/index.html) which is created,
maintained and hosted by [Simon
Norris](https://github.com/smnorris/fwapg) at [Hillcrest
Geographics](https://hillcrestgeo.ca/main/).

## Contribution

Please report any
[issues](https://github.com/poissonconsulting/fwatlasbc/issues).

[Pull requests](https://github.com/poissonconsulting/fwatlasbc/pulls)
are always welcome.

### Code of Conduct

Please note that `fwatlasbc` is released with a [Contributor Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
