#' Flip Streams
#'
#' Ensures that stream linestrings flow in a downstream direction, i.e.,
#' that the start of each linestring is higher than its end.
#' The elevations of the start and end of each linestring are obtained
#' from Google Maps using [`fwa_add_gm_elevation_to_point()`] and
#' linestrings whose start is lower than their end are reversed.
#' Linestrings whose start and end have the same elevation are not altered.
#'
#' @param x An sf tibble with a column blk and linestrings of streams
#' (or stream segments).
#' @return A copy of `x` with the linestrings flowing downstream.
#' @export
#' @seealso [`fwa_add_gm_elevation_to_point()`] and
#' [`fwa_join_stream_segments()`]
#' @examples
#' \dontrun{
#' watershed <- fwa_add_watershed_to_blk(data.frame(blk = 356308001, rm = 1000))
#' network <- fwa_add_collection_to_polygon(watershed)
#' network <- select(network, blk = blue_line_key)
#' fwa_flip_streams(network)
#' }
fwa_flip_streams <- function(x) {
  chk_s3_class(x, "sf")
  check_names(x, "blk")
  chk_s3_class(sf::st_geometry(x), "sfc_LINESTRING")

  chk_whole_numeric(x$blk)
  chk_not_any_na(x$blk)
  chk_gt(x$blk)

  if (!nrow(x)) {
    return(x)
  }

  geometry <- sf::st_geometry(x)
  n <- length(geometry)

  points <- sf::st_sf(
    geometry = c(
      sf::st_line_sample(geometry, sample = 0),
      sf::st_line_sample(geometry, sample = 1)
    ) |>
      sf::st_cast("POINT")
  )

  elevation <- fwa_add_gm_elevation_to_point(points)$elevation
  flip <- elevation[seq_len(n)] < elevation[n + seq_len(n)]

  geometry[flip] <- geometry[flip] |>
    purrr::map(reverse_linestring) |>
    sf::st_sfc(crs = sf::st_crs(geometry))

  sf::st_set_geometry(x, geometry)
}
