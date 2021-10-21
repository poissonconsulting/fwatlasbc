
<!-- README.md is generated from README.Rmd. Please edit that file -->

# fwatlasbc <img src="man/figures/logo.png" align="right" />

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/poissonconsulting/fwatlasbc/workflows/R-CMD-check/badge.svg)](https://github.com/poissonconsulting/fwatlasbc/actions)
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
#>   stream_name       
#>   <chr>             
#> 1 Steep Canyon Creek
#> 2 Steep Creek
```

Add blue line keys to stream names.

``` r
streams <- fwa_add_blks_to_stream_name(streams)
streams
#> # A tibble: 4 × 2
#>   stream_name              blk
#>   <chr>                  <int>
#> 1 Steep Canyon Creek 360883036
#> 2 Steep Creek        356362258
#> 3 Steep Creek        356534225
#> 4 Steep Creek        356570155
streams <- streams[streams$blk == 356534225,]
```

Get river meters (every 100 m).

``` r
rm <- fwa_add_rms_to_blk(streams, interval = 100)
rm
#> Simple feature collection with 46 features and 4 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 1657747 ymin: 728476.5 xmax: 1661313 ymax: 730795.9
#> Projected CRS: NAD83 / BC Albers
#> # A tibble: 46 × 5
#>    stream_name       blk    rm elevation           geometry
#>    <chr>           <int> <int>     <dbl>        <POINT [m]>
#>  1 Steep Creek 356534225     0     1087  (1657747 728476.5)
#>  2 Steep Creek 356534225   100     1094. (1657839 728506.7)
#>  3 Steep Creek 356534225   200     1102. (1657911 728572.2)
#>  4 Steep Creek 356534225   300     1111. (1657989 728633.1)
#>  5 Steep Creek 356534225   400     1124.   (1658070 728691)
#>  6 Steep Creek 356534225   500     1133. (1658141 728754.7)
#>  7 Steep Creek 356534225   600     1145. (1658233 728792.8)
#>  8 Steep Creek 356534225   700     1154. (1658324 728830.6)
#>  9 Steep Creek 356534225   800     1168. (1658395 728897.5)
#> 10 Steep Creek 356534225   900     1181. (1658470 728960.1)
#> # … with 36 more rows
```

Or get a blue line key and river meter from longitude and latitude.

``` r
fwa_add_rm_to_lon_lat(data.frame(lon = -132.26, lat = 53.36))
#> Simple feature collection with 1 feature and 5 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 585153.6 ymin: 946162.9 xmax: 585153.6 ymax: 946162.9
#> Projected CRS: NAD83 / BC Albers
#> # A tibble: 1 × 6
#>     lon   lat       blk    rm distance_to_lon_lat            geometry
#>   <dbl> <dbl>     <int> <dbl>               <dbl>         <POINT [m]>
#> 1 -132.  53.4 360824839 1118.                508. (585153.6 946162.9)
```

Get watershed.

``` r
wshed <- fwa_add_watershed_to_blk(streams)
wshed
#> Simple feature collection with 1 feature and 3 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 1656218 ymin: 725423.1 xmax: 1661726 ymax: 732146.2
#> Projected CRS: NAD83 / BC Albers
#> # A tibble: 1 × 4
#>   stream_name       blk    rm                                           geometry
#>   <chr>           <int> <dbl>                                      <POLYGON [m]>
#> 1 Steep Creek 356534225     0 ((1658037 728924.8, 1658107 728964.9, 1658107 728…
```

Get stream network

``` r
stream_network <- fwa_add_collection_to_watershed(wshed, "stream_network")
stream_network
#> Simple feature collection with 76 features and 31 fields
#> Geometry type: LINESTRING
#> Dimension:     XY
#> Bounding box:  xmin: 1656200 ymin: 725423.1 xmax: 1661368 ymax: 731602.7
#> Projected CRS: NAD83 / BC Albers
#> # A tibble: 76 × 32
#>    stream_name       blk    rm id        blue_line_key blue_line_key_50k
#>    <chr>           <int> <dbl> <chr>             <int>             <int>
#>  1 Steep Creek 356534225     0 707009047     356499676                NA
#>  2 Steep Creek 356534225     0 707009141     356407032                NA
#>  3 Steep Creek 356534225     0 707009211     356462244                NA
#>  4 Steep Creek 356534225     0 707009213     356494188                NA
#>  5 Steep Creek 356534225     0 707009233     356499676                NA
#>  6 Steep Creek 356534225     0 707009235     356397697                NA
#>  7 Steep Creek 356534225     0 707009415     356499676                NA
#>  8 Steep Creek 356534225     0 707009417     356494188                NA
#>  9 Steep Creek 356534225     0 707009460     356499676                NA
#> 10 Steep Creek 356534225     0 707009535     356499676                NA
#> # … with 66 more rows, and 26 more variables: downstream_route_measure <dbl>,
#> #   edge_type <int>, feature_code <chr>, feature_source <chr>,
#> #   fwa_watershed_code <chr>, gnis_id <int>, gnis_name <chr>, gradient <dbl>,
#> #   left_right_tributary <chr>, length_metre <dbl>, linear_feature_id <int>,
#> #   local_watershed_code <chr>, localcode_ltree <chr>, stream_magnitude <int>,
#> #   stream_order <int>, upstream_area_ha <chr>, upstream_route_measure <dbl>,
#> #   waterbody_key <int>, watershed_code_50k <chr>, …
```

Plot watershed and river meters.

``` r
ggplot2::ggplot() +
  ggplot2::geom_sf(data = wshed) +
  ggplot2::geom_sf(data = stream_network, color = "blue") +
  ggplot2::geom_sf(data = rm)
```

<img src="man/figures/README-unnamed-chunk-8-1.png" width="100%" />

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
