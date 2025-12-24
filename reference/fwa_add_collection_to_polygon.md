# Add Collection to Polygon

Adds collection to a polygon such as a watershed. If the active sfc
polygon column is called geometry it is replaced by the geometry column
of the collection. If the collection includes a blue_line_key column the
values are copied to column blk replacing any existing values.

## Usage

``` r
fwa_add_collection_to_polygon(
  x,
  collection = "stream_network",
  intersect = FALSE,
  filter = NULL,
  limit = 10000,
  offset = 0,
  properties = NULL,
  transform = NULL,
  epsg = getOption("fwa.epsg", 3005),
  nocache = getOption("fwa.nocache", FALSE)
)

fwa_add_collection_to_watershed(
  x,
  collection = "stream_network",
  intersect = FALSE,
  filter = NULL,
  limit = 10000,
  offset = 0,
  properties = NULL,
  transform = NULL,
  epsg = getOption("fwa.epsg", 3005),
  nocache = getOption("fwa.nocache", FALSE)
)
```

## Arguments

- x:

  A sf object with an active sfc polygon column.

- collection:

  A character string of the collection.

- intersect:

  A logical vector specifying whether to intersect the individual
  features with the polygon as opposed to just including the features
  that intersect it.

- filter:

  A named vector or list of the filter(s) to apply, where the list names
  correspond to column names and the list values correspond to the
  desired value, e.g. `list(gnis_name = "Sangan River")`.

- limit:

  A positive whole number indicating the maximum number of features to
  return.

- offset:

  A positive whole number indicating the offset of start of returned
  results.

- properties:

  A vector of strings of the column names to include. If NULL (default),
  all columns are retained.

- transform:

  A character vector with the name of the [*valid
  transform*](https://github.com/CrunchyData/pg_featureserv/blob/master/config/pg_featureserv.toml.example#L29)
  function followed by the parameter values (e.g. c("ST_Simplify", 100))

- epsg:

  A positive whole number of the epsg to transform features to.

- nocache:

  A flag specifying whether or not to cache results.

## Value

An sf object

## Functions

- `fwa_add_collection_to_watershed()`: **\[superseded\]**

  Deprecated for `fwa_add_collection_to_polygon()`.

## See also

[`fwapgr::fwa_collection()`](https://rdrr.io/pkg/fwapgr/man/fwa_query_collection.html).

## Examples

``` r
if (FALSE) { # \dontrun{
watershed <- fwa_add_watershed_to_blk(data.frame(blk = 356308001))
fwa_add_collection_to_polygon(watershed)
} # }
```
