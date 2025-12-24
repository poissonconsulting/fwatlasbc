# Collection Name

A tibble of collection names.

## Usage

``` r
fwa_collection_name
```

## Format

An object of class `spec_tbl_df` (inherits from `tbl_df`, `tbl`,
`data.frame`) with 12 rows and 2 columns.

## Examples

``` r
fwa_collection_name
#> # A tibble: 12 × 2
#>    collection_name     collection                                   
#>    <chr>               <chr>                                        
#>  1 cultural_lines      whse_basemapping.trim_cultural_lines         
#>  2 cultural_points     whse_basemapping.trim_cultural_points        
#>  3 lakes               whse_basemapping.fwa_lakes_poly              
#>  4 manmade_waterbodies whse_basemapping.fwa_manmade_waterbodies_poly
#>  5 named_streams       whse_basemapping.fwa_named_streams           
#>  6 obstructions        whse_basemapping.fwa_obstructions_sp         
#>  7 railway_tracks      whse_basemapping.gba_railway_tracks_sp       
#>  8 rivers              whse_basemapping.fwa_rivers_poly             
#>  9 stream_network      whse_basemapping.fwa_stream_networks_sp      
#> 10 transmission_lines  whse_basemapping.gba_transmission_lines_sp   
#> 11 transport_lines     whse_basemapping.transport_line              
#> 12 wetlands            whse_basemapping.fwa_wetlands_poly           
```
