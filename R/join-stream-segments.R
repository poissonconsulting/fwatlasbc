join_string <- function(x) {
  dplyr::as_tibble(x) |>
    dplyr::select("blk") |>
    dplyr::distinct() |>
    dplyr::bind_cols(geometry = sf::st_union(x)) |>
    sf::st_sf() |>
    sf::st_cast("MULTILINESTRING") |>
    sf::st_line_merge() |>
    dplyr::bind_cols()
}

join_strings <- function(x) {
  x |>
    sf::st_zm() |>
    sf::st_sf() |>
    dplyr::group_split(.data$blk) |>
    purrr::map(join_string) |>
    dplyr::bind_rows() |>
    sf::st_sf() |>
    dplyr::filter(purrr::map_lgl(.data$geometry, is_linestring))
}

start_end_elevation <- function(x) {
  crs <- sf::st_crs(x)

  x <- x |>
    sf::st_sf(sf_column_name = "start") |>
    fwa_add_gm_elevation_to_point() |>
    dplyr::rename(start_elevation = "elevation") |>
    sf::st_sf(sf_column_name = "end") |>
    fwa_add_gm_elevation_to_point() |>
    dplyr::rename(end_elevation = "elevation") |>
    dplyr::mutate(reverse = .data$start_elevation > .data$end_elevation) |>
    sf::st_sf()

  x |>
    dplyr::filter(.data$reverse) |>
    reverse_linestrings() |>
    dplyr::bind_rows(dplyr::filter(x, !.data$reverse)) |>
    dplyr::select(!c("start_elevation", "end_elevation", "reverse")) |>
    sf::st_set_crs(crs)
}

start_points <- function(x, elevation) {
  x <- x |>
    dplyr::mutate(start = sf::st_line_sample(x, sample = 0),
                  end = sf::st_line_sample(x, sample = 1),
                  start = sf::st_cast(.data$start, "POINT"),
                  end = sf::st_cast(.data$end, "POINT"))

  if(elevation) {
    x <- x |>
      start_end_elevation()
  }
  x |>
    dplyr::select(!c("end", "start"))
}

#' Join Stream Segments
#'
#' Converts a tibble of stream segment linestrings to stream linestrings.
#'
#' @param x An sf tibble with a column blk and linestrings of stream segments.
#' @param elevation A flag specifying whether to use the elevation
#' from Google Maps to determine stream direction (or use the
#' direction of the provided linestrings)
#' @return An sf tibble with the columns blk and sfc column point geometry.
#' @export
#' @seealso [`fwa_convert_streams_to_rms()`]
#' @examples
#' \dontrun{
#' watershed <- fwa_add_watershed_to_blk(data.frame(blk = 356308001, rm = 1000))
#' network <- fwa_add_collection_to_polygon(watershed)
#' network <- select(network, blk = blue_line_key)
#' fwa_join_stream_segments(network)
#' }
fwa_join_stream_segments <- function(x, elevation = FALSE) {
  chk_s3_class(x, "sf")
  check_names(x, "blk")

  chk_flag(elevation)

  x |>
    join_strings() |>
    start_points(elevation)
}
