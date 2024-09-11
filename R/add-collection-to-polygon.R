rename_collection <- function(collection) {
  chk_string(collection) # +chk

  wch <- which(fwatlasbc::fwa_collection_name$collection_name == collection)
  if (!length(wch)) return(collection)
  fwatlasbc::fwa_collection_name$collection[wch]
}

add_collection_to_polygon <- function(x, collection, filter, limit, offset,
                                      properties, transform, epsg, nocache) {
  check_dim(x, dim = nrow, values = 1L) # +chk

  polygon <- sf::st_geometry(x)

  bbox <- bbox(polygon)

  coll <- fwapgr::fwa_query_collection(collection,
    filter = filter,
    limit = limit,
    offset = offset,
    bbox = bbox,
    properties = properties,
    transform = transform,
    epsg = epsg,
    nocache = nocache
  )

  polygon <- sf::st_transform(polygon, epsg)
  polygon <- sf::st_make_valid(polygon)
  suppressMessages(coll <- coll[sf::st_intersects(coll, polygon, sparse = FALSE)[, 1], ])
  coll <- sf::st_make_valid(coll)
  if (x$..fwa_intersect) {
    suppressWarnings(coll <- sf::st_intersection(coll, polygon, validate = TRUE))
    coll <- sf::st_make_valid(coll)
  }
  coll <- coll |>
    dplyr::mutate(dplyr::across(dplyr::matches("gnis_id_\\d"), as.integer)) |>
    dplyr::as_tibble()

  x <- x |>
    dplyr::as_tibble() |>
    dplyr::mutate(geometry = NULL) |>
    dplyr::bind_cols(coll) |>
    sf::st_set_geometry("geometry")
}

#' Add Collection to Polygon
#'
#' Adds collection to a polygon such as a watershed.
#' If the active sfc polygon column is called geometry it is replaced
#' by the geometry column of the collection.
#' If the collection includes a blue_line_key column the values are
#' copied to column blk replacing any existing values.
#'
#' @inheritParams fwapgr::fwa_collection
#' @param x A sf object with an active sfc polygon column.
#' @param collection A character string of the collection.
#' @param intersect A logical vector specifying whether to intersect the
#' individual features with the polygon as opposed to just including
#' the features that intersect it.
#' @return An sf object
#' @seealso [fwapgr::fwa_collection()].
#' @export
#' @examples
#' \dontrun{
#' watershed <- fwa_add_watershed_to_blk(data.frame(blk = 356308001))
#' fwa_add_collection_to_polygon(watershed)
#' }
fwa_add_collection_to_polygon <- function(
    x, collection = "stream_network",
    intersect = FALSE,
    filter = NULL,
    limit = 10000,
    offset = 0,
    properties = NULL,
    transform = NULL,
    epsg = getOption("fwa.epsg", 3005),
    nocache = getOption("fwa.nocache", FALSE)) {
  chk_s3_class(x, "sf")
  chk_s3_class(sf::st_geometry(x), "sfc_POLYGON")

  check_dim(x, nrow, TRUE)
  chk_string(collection)
  chk_logical(intersect)
  chk_not_any_na(intersect)
  chk_not_subset(colnames(x), c("..fwa_id", "..fwa_intersect"))

  collection <- rename_collection(collection)

  x <- x |>
    dplyr::mutate(
      ..fwa_id = 1:dplyr::n(),
      ..fwa_intersect = intersect
    ) |>
    group_split_sf(.data$..fwa_id) |>
    lapply(add_collection_to_polygon,
      collection = collection,
      filter = filter, limit = limit,
      offset = offset, properties = properties, transform = transform,
      epsg = epsg, nocache = nocache
    ) |>
    dplyr::bind_rows()

  if ("blue_line_key" %in% colnames(x)) {
    x <- x |> mutate(blk = .data$blue_line_key)
  }

  x |>
    dplyr::arrange(.data$..fwa_id) |>
    dplyr::select(!c("..fwa_id", "..fwa_intersect"))
}
