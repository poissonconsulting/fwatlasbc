# Find Stream Names

Finds gnis stream names that match regular expression.

## Usage

``` r
fwa_find_stream_names(pattern = ".*", ignore_case = TRUE)
```

## Arguments

- pattern:

  A string of a regular expression.

- ignore_case:

  A flag specifying whether to ignore case when matching the regular
  expression to gnis stream names.

## Value

A tibble with character column stream_name of the names of all the
streams that match the regular expression.

## Examples

``` r
fwa_find_stream_names("sangan")
#> # A tibble: 1 × 1
#>   stream_name 
#>   <chr>       
#> 1 Sangan River
```
