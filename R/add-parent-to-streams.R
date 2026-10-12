#' Add Parent to Streams
#'
#' Adds the parent blue line key (`parent_blk`) and the river meter on the
#' parent where the stream joins it (`parent_rm`) to stream linestrings.
#' The parent is the nearest other stream to the mouth of each stream.
#' If the mouth is more than `gap` m from the nearest other stream then
#' `parent_blk` and `parent_rm` are `NA`.
#'
#' The stream segments for each blk are first joined and oriented using
#' [`fwa_join_stream_segments()`] so that the mouth is the start of each
#' stream and river meters on the parent are measured from its mouth.
#' The geometry of `x` is not altered.
#'
#' @inheritParams fwa_convert_streams_to_rms
#' @param x An sf tibble with a column blk and linestrings of streams
#' (or stream segments).
#' @return A copy of `x` with integer column `parent_blk` and
#' numeric column `parent_rm`.
#' @export
#' @seealso [`fwa_convert_streams_to_rms()`]
#' @examples
#' \dontrun{
#' watershed <- fwa_add_watershed_to_blk(data.frame(blk = 356308001, rm = 1000))
#' network <- fwa_add_collection_to_polygon(watershed)
#' network <- select(network, blk = blue_line_key)
#' fwa_add_parent_to_streams(network)
#' }
fwa_add_parent_to_streams <- function(
  x,
  gap = 1,
  elevation = FALSE,
  reverse = integer(),
  source = "elevatr",
  zoom = 12
) {
  chk_s3_class(x, "sf")
  check_names(x, "blk")
  chk_not_subset(colnames(x), c("parent_blk", "parent_rm"))

  chk_whole_numeric(x$blk)
  chk_not_any_na(x$blk)
  chk_gt(x$blk)

  chk_number(gap)
  chk_gte(gap)

  chk_flag(elevation)

  chk_whole_numeric(reverse)
  chk_not_any_na(reverse)
  chk_gt(reverse)
  chk_unique(reverse)
  chk_subset(reverse, x$blk)

  chk_string(source)
  chk_subset(source, c("elevatr", "google"))

  chk_whole_number(zoom)
  chk_range(zoom, c(1, 14))

  crs <- sf::st_crs(x)

  streams <- x |>
    fwa_join_stream_segments(elevation, reverse, source, zoom) |>
    sf::st_set_crs(crs)

  mouths <- streams |>
    sf::st_set_geometry(
      sf::st_line_sample(sf::st_geometry(streams), sample = 0) |>
        sf::st_cast("POINT")
    ) |>
    dplyr::mutate(rm = 0L)

  sf::st_geometry(mouths) <- "geometry"

  parents <- mouths |>
    get_parent_stream(streams, gap = gap) |>
    dplyr::as_tibble() |>
    dplyr::distinct(.data$blk, .data$parent_blk, .data$parent_rm) |>
    dplyr::mutate(parent_blk = as.integer(.data$parent_blk))

  x |>
    dplyr::left_join(parents, by = "blk")
}
