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
  x <- x |>
    sf::st_sf(sf_column_name = "start") |>
    fwa_add_gm_elevation_to_point() |>
    dplyr::rename(start_elevation = .data$elevation) |>
    sf::st_sf(sf_column_name = "end") |>
    fwa_add_gm_elevation_to_point() |>
    dplyr::rename(end_elevation = .data$elevation) |>
    dplyr::mutate(reverse = .data$start_elevation > .data$end_elevation) |>
    sf::st_sf()

  x |>
    dplyr::filter(.data$reverse) |>
    reverse_linestrings() |>
    dplyr::bind_rows(dplyr::filter(x, !.data$reverse)) |>
    dplyr::select(!c("start_elevation", "end_elevation", "reverse")) |>
    identity()
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
    dplyr::select(!"end")
}

#' Convert Streams to River Meters
#'
#' Converts a tibble of streams to river meters.
#' Unlike [`fwa_convert_stream_network_to_rms()`] it only requires
#' the linestrings and the unique integer identifier for each stream.
#'
#' @param x An sf tibble with a column blk and linestrings of streams.
#' @param interval A positive whole number of the distance (m) between points.
#' @param gap A positive real number specifying the maximum gap between
#' the mouth of stream and its parent stream to be considered connected.
#' @param elevation A flag specifying whether to use the elevation
#' from Google Maps to determine stream direction (or use the
#' direction of the provided linestrings)
#' @return An sf tibble with the columns blk, integer column rm
#' and sf column point geometry.
#' @export
#' @seealso [`fwa_convert_stream_network_to_rms()`]
#' @examples
#' \dontrun{
#' watershed <- fwa_add_watershed_to_blk(data.frame(blk = 356308001, rm = 1000))
#' network <- fwa_add_collection_to_polygon(watershed)
#' network <- select(network, blk = blue_line_key)
#' fwa_convert_streams_to_rms(network, interval = 100)
#' }
fwa_convert_streams_to_rms <- function(x, interval = 5, gap = 1, elevation = FALSE) {
  chk_s3_class(x, "sf")
  chk_whole_number(interval)
  chk_gt(interval)

  check_names(x, "blk")
  chk_not_subset(colnames(x), "..fwa_length")

  chk_whole_numeric(x$blk)
  chk_not_any_na(x$blk)
  chk_gt(x$blk)

  chk_flag(elevation)

  crs <- sf::st_crs(x)

  x |>
    join_strings() |>
    start_points(elevation) |>
    sample_linestrings(interval) |>
    dplyr::select(!"start") |>
    sf::st_set_crs(crs)
}
