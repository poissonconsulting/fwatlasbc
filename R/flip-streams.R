#' Flip Streams
#'
#' Ensures that stream linestrings are oriented in an upstream direction, i.e.,
#' that the start (mouth) of each linestring is lower than its end.
#' The elevations of the start and end of each linestring are obtained
#' using [`fwa_add_elevatr_elevation_to_point()`] or
#' [`fwa_add_gm_elevation_to_point()`] and
#' linestrings whose start is more than `min_difference` m higher than their
#' end are reversed.
#' All other linestrings are not altered.
#'
#' @param x An sf tibble with a column blk and linestrings of streams
#' (or stream segments).
#' @param source A string of the source of the elevations.
#' Possible values are `"elevatr"` (no API key required) and
#' `"google"` (requires a Google Maps Elevation API key).
#' If `NULL` (the default) `"google"` is used but this is
#' `r lifecycle::badge("deprecated")` and `source` should be set explicitly.
#' @param min_difference A non-negative number of the minimum difference (m)
#' between the elevations of the start and end of a linestring
#' for it to be reversed.
#' @param zoom A whole number between 1 and 14 of the zoom level of the
#' Terrain Tiles.
#' Higher zoom levels have a finer resolution but take longer to download.
#' Ignored if `source = "google"`.
#' @return A copy of `x` with the linestrings oriented upstream.
#' @export
#' @seealso [`fwa_add_elevatr_elevation_to_point()`],
#' [`fwa_add_gm_elevation_to_point()`] and [`fwa_join_stream_segments()`]
#' @examples
#' \dontrun{
#' watershed <- fwa_add_watershed_to_blk(data.frame(blk = 356308001, rm = 1000))
#' network <- fwa_add_collection_to_polygon(watershed)
#' network <- select(network, blk = blue_line_key)
#' fwa_flip_streams(network)
#' }
fwa_flip_streams <- function(
  x,
  source = NULL,
  zoom = 12,
  min_difference = 5
) {
  chk_s3_class(x, "sf")
  check_names(x, "blk")
  chk_s3_class(sf::st_geometry(x), "sfc_LINESTRING")

  chk_whole_numeric(x$blk)
  chk_not_any_na(x$blk)
  chk_gt(x$blk)

  source <- check_source(source, "fwa_flip_streams")

  chk_whole_number(zoom)
  chk_range(zoom, c(1, 14))

  chk_number(min_difference)
  chk_gte(min_difference)

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

  elevation <- add_elevation_to_point(points, source, zoom)$elevation
  flip <- elevation[seq_len(n)] - elevation[n + seq_len(n)] > min_difference
  flip <- flip & !is.na(flip)

  geometry[flip] <- geometry[flip] |>
    purrr::map(reverse_linestring) |>
    sf::st_sfc(crs = sf::st_crs(geometry))

  sf::st_set_geometry(x, geometry)
}
