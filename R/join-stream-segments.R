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

start_end_elevation <- function(x, elevation, reverse, source) {
  crs <- sf::st_crs(x)

  x_swap <- x |>
    dplyr::filter(.data$blk %in% reverse) |>
    dplyr::mutate(reverse = TRUE)

  if (!elevation) {
    x <- x |>
      dplyr::filter(!.data$blk %in% reverse) |>
      dplyr::mutate(reverse = FALSE) |>
      dplyr::bind_rows(x_swap)
  } else {
    x <- x |>
      dplyr::filter(!.data$blk %in% reverse) |>
      sf::st_sf(sf_column_name = "start") |>
      add_elevation_to_point(source) |>
      ## as_tibble() necessary to avoid internal error: can't find `agr` columns on rename
      as_tibble() |>
      dplyr::rename(start_elevation = "elevation") |>
      sf::st_sf(sf_column_name = "end") |>
      add_elevation_to_point(source) |>
      ## as_tibble() necessary to avoid internal error: can't find `agr` columns on rename
      as_tibble() |>
      dplyr::rename(end_elevation = "elevation") |>
      dplyr::mutate(reverse = .data$start_elevation > .data$end_elevation) |>
      sf::st_sf() |>
      dplyr::bind_rows(x_swap)
  }

  x |>
    dplyr::filter(.data$reverse) |>
    reverse_linestrings() |>
    dplyr::bind_rows(dplyr::filter(x, !.data$reverse)) |>
    dplyr::select(
      !dplyr::any_of(c("start_elevation", "end_elevation", "reverse"))
    ) |>
    sf::st_set_crs(crs)
}

start_points <- function(x, elevation, reverse, source) {
  x <- x |>
    dplyr::mutate(
      start = sf::st_line_sample(x, sample = 0),
      end = sf::st_line_sample(x, sample = 1),
      start = sf::st_cast(.data$start, "POINT"),
      end = sf::st_cast(.data$end, "POINT")
    )

  if (elevation || length(reverse)) {
    x <- x |>
      start_end_elevation(elevation, reverse, source)
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
#' to determine stream direction (or use the
#' direction of the provided linestrings)
#' @param reverse A whole numeric vector of streams to reverse direction
#' ignoring elevation.
#' @param source A string of the source of the elevations if
#' `elevation = TRUE`.
#' Possible values are `"elevatr"` (no API key required) and
#' `"google"` (requires a Google Maps Elevation API key).
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
fwa_join_stream_segments <- function(
  x,
  elevation = FALSE,
  reverse = integer(),
  source = "elevatr"
) {
  chk_s3_class(x, "sf")
  check_names(x, "blk")

  check_names(x, "blk")

  chk_whole_numeric(x$blk)
  chk_not_any_na(x$blk)
  chk_gt(x$blk)

  chk_flag(elevation)

  chk_whole_numeric(reverse)
  chk_not_any_na(reverse)
  chk_gt(reverse)
  chk_unique(reverse)
  chk_subset(reverse, x$blk)

  chk_string(source)
  chk_subset(source, c("elevatr", "google"))

  x |>
    join_strings() |>
    start_points(elevation, reverse, source)
}
