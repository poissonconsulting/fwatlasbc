rename_collection <- function(collection) {
  chk_string(collection) # +chk

  wch <- which(fwatlasbc::fwa_collection_name$collection_name == collection)
  if(!length(wch)) return(collection)
  fwatlasbc::fwa_collection_name$collection[wch]
}

add_collection_to_watershed <- function(x, collection, filter, limit, offset,
                                        properties, transform, epsg) {
  check_dim(x, dim = nrow, values = 1L) # +chk

  watershed <- sf::st_geometry(x)

  chk_s3_class(watershed, "sfc_POLYGON", x_name = "`x` active sfc column")

  bbox <- bbox(watershed)

  coll <- fwapgr::fwa_query_collection(collection,
                                       filter = filter,
                                       limit = limit,
                                       offset = offset,
                                       bbox = bbox,
                                       properties = properties,
                                       transform = transform,
                                       epsg = epsg)

  watershed <- sf::st_transform(watershed, epsg)
  watershed <- sf::st_make_valid(watershed)
  suppressMessages(coll <- coll[sf::st_intersects(coll, watershed, sparse = FALSE)[,1],])
  coll <- sf::st_make_valid(coll)
  if(x$..fwa_intersect) {
    suppressWarnings(coll <- sf::st_intersection(coll, watershed, validate = TRUE))
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

#' Add Collection to Watershed
#'
#' Adds collection to a watershed.
#' If the active sfc polygon column is called geometry it is replaced
#' by the geometry column of the collection.
#' The collection includes a blue_line_key column the values are
#' copied to column blk replacing any existing values.
#'
#' @inheritParams fwapgr::fwa_collection
#' @param x A sf object with an active sfc polygon column.
#' @param collection A character string of the collection.
#' @param intersect A logical vector specifying whether to intersect the
#' individual features with the watershed as opposed to just including
#' the features that intersect the watershed.
#' @return An sf object
#' @seealso \code{\link[fwapgr]{fwa_collection}}.
#' @export
#' @examples
#' \dontrun{
#' watershed <- fwa_add_watershed_to_blk(data.frame(blk = 356308001))
#' fwa_add_collection_to_watershed(watershed)
#' }
fwa_add_collection_to_watershed <- function(x, collection = "stream_network",
                                            intersect = FALSE,
                                            filter = NULL,
                                            limit = 10000,
                                            offset = 0,
                                            properties = NULL,
                                            transform = NULL,
                                            epsg = getOption("fwa.epsg", 3005)) {
  chk_s3_class(x, "sf")
  check_dim(x, nrow, TRUE)
  chk_string(collection)
  chk_logical(intersect)
  chk_not_any_na(intersect)
  chk_not_subset(colnames(x), c("..fwa_id", "..fwa_intersect"))

  collection <- rename_collection(collection)

  x <- x |>
    dplyr::mutate(..fwa_id = 1:dplyr::n(),
                  ..fwa_intersect = intersect) |>
    dplyr::group_split(.data$..fwa_id) |>
    lapply(add_collection_to_watershed, collection = collection,
           filter = filter, limit = limit,
           offset = offset, properties = properties, transform = transform,
           epsg = epsg) |>
    dplyr::bind_rows()

  if("blue_line_key" %in% colnames(x)) {
    x <- x |> mutate(blk = .data$blue_line_key)
  }

  x |>
    dplyr::arrange(.data$..fwa_id) |>
    dplyr::select(-.data$..fwa_id, -.data$..fwa_intersect)
}
