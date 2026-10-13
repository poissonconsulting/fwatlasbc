#' Reverse Streams
#'
#' Reverses the direction of the linestrings of the streams with the
#' specified blue line keys.
#' All other linestrings are not altered.
#'
#' @param x An sf tibble with a column blk and linestrings of streams
#' (or stream segments).
#' @param blk A whole numeric vector of the unique blue line keys of the
#' streams to reverse.
#' All values must be in `x$blk`.
#' @return A copy of `x` with the linestrings of the specified streams
#' reversed.
#' @export
#' @seealso [`fwa_flip_streams()`] and [`fwa_mapview_streams()`]
#' @examples
#' \dontrun{
#' watershed <- fwa_add_watershed_to_blk(data.frame(blk = 356308001, rm = 1000))
#' network <- fwa_add_collection_to_polygon(watershed)
#' network <- select(network, blk = blue_line_key)
#' fwa_reverse_streams(network, 356308001)
#' }
fwa_reverse_streams <- function(x, blk) {
  chk_s3_class(x, "sf")
  check_names(x, "blk")
  chk_s3_class(sf::st_geometry(x), "sfc_LINESTRING")

  chk_whole_numeric(x$blk)
  chk_not_any_na(x$blk)
  chk_gt(x$blk)

  chk_whole_numeric(blk)
  chk_not_any_na(blk)
  chk_gt(blk)
  chk_unique(blk)
  chk_subset(blk, x$blk)

  geometry <- sf::st_geometry(x)
  reverse <- x$blk %in% blk

  geometry[reverse] <- geometry[reverse] |>
    purrr::map(reverse_linestring) |>
    sf::st_sfc(crs = sf::st_crs(geometry))

  sf::st_set_geometry(x, geometry)
}
