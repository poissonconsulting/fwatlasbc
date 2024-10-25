#' @describeIn fwa_add_collection_to_polygon
#'
#' `r lifecycle::badge('superseded')`
#'
#' Deprecated for `fwa_add_collection_to_polygon()`.
#'
fwa_add_collection_to_watershed <- function(
    x, collection = "stream_network",
    intersect = FALSE,
    filter = NULL,
    limit = 10000,
    offset = 0,
    properties = NULL,
    transform = NULL,
    epsg = getOption("fwa.epsg", 3005),
    nocache = getOption("fwa.nocache", FALSE)) {
  lifecycle::deprecate_soft(
    "0.0.2", "fwa_add_collection_to_watershed()",
    "fwa_add_collection_to_polygon()"
  )

  fwa_add_collection_to_polygon(
    x = x, collection = collection,
    intersect = intersect, filter = filter,
    limit = limit, offset = offset,
    properties = properties,
    transform = transform, epsg = epsg, nocache = nocache
  )
}
