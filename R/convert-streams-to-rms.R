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

get_parent_blk <- function(x, y) {
  y <- y |>
    dplyr::anti_join(as_tibble(x), by = "blk")

  x |>
    dplyr::mutate(parent_blk = sf::st_nearest_feature(x, y),
                  parent_blk = y$blk[.data$parent_blk])
}

get_parent_rm <- function(x) {
  x |>
    dplyr::mutate(parent_rm = lwgeom::st_split(.data$..fwa_linestring, .data$parent_rm),
                  parent_rm = sf::st_collection_extract(.data$parent_rm,"LINESTRING"),
                  parent_rm = sf::st_length(.data$parent_rm),
                  parent_rm = as.numeric(parent_rm))
}

get_parent_stream <- function(x, y, gap) {
  sf::st_geometry(y) <- "..fwa_linestring"

  mouth <- x |>
    dplyr::filter(rm == 0) |>
    dplyr::group_split(.data$blk) |>
    purrr::map(get_parent_blk, y) |>
    dplyr::bind_rows() |>
    dplyr::left_join(as_tibble(y), by = "blk") |>
    dplyr::mutate(parent_rm = sf::st_nearest_points(.data$geometry, .data$..fwa_linestring, pairwise = TRUE),
                  ..fwa_distance = as.numeric(sf::st_length(.data$parent_rm)),
                  parent_rm = sf::st_line_sample(.data$parent_rm, 1),
                  parent_rm = sf::st_cast(.data$parent_rm, "POINT")) |>
    dplyr::group_split(.data$blk) |>
    purrr::map(get_parent_rm) |>
    dplyr::bind_rows() |>
    dplyr::mutate(parent_blk = dplyr::if_else(.data$..fwa_distance > gap, NA_integer_, .data$parent_blk),
                 parent_rm = dplyr::if_else(.data$..fwa_distance > gap, NA_real_, .data$parent_rm)) |>
    as_tibble() |>
    dplyr::select("blk", "parent_blk", "parent_rm")

  x |>
    left_join(mouth, by = "blk")
}
#
#
# > parts = st_collection_extract(st_split(reach$geometry, site_snap$geometry),"LINESTRING")
#

#' Convert Streams to River Meters
#'
#' Converts a tibble of streams to river meters.
#' Unlike [`fwa_convert_stream_network_to_rms()`] it only requires
#' the linestrings and the unique integer identifier for each stream.
#'
#' @param x An sf tibble with a column blk and linestrings of streams.
#' @param interval A positive whole number of the distance (m) between points.
#' @param gap A positive real number specifying the maximum gap (m) between
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

  x <- x |>
    join_strings()

  x |>
    start_points(elevation) |>
    sample_linestrings(interval) |>
    get_parent_stream(x, gap = gap) |>
    sf::st_set_crs(crs)
}
