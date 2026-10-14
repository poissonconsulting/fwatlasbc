#' Add Parent to Streams
#'
#' Adds the parent blue line key (`parent_blk`) and the river meter on the
#' parent where the stream joins it (`parent_rm`) to stream linestrings.
#' The parent is the nearest other stream to the mouth of each stream.
#' If the mouth is more than `gap` m from the nearest other stream then
#' `parent_blk` and `parent_rm` are `NA`.
#'
#' The stream segments for each blk are first joined using
#' [`fwa_join_stream_segments()`].
#' The start of each joined stream is taken to be its mouth and river meters on
#' the parent are measured from its start.
#' Unless `elevation = TRUE`, streams digitized from source to mouth must first
#' be reversed using [`fwa_reverse_streams()`].
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
  source = NULL,
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

  source <- check_source(source, "fwa_add_parent_to_streams", warn = elevation)

  chk_whole_number(zoom)
  chk_range(zoom, c(1, 14))

  crs <- sf::st_crs(x)

  streams <- x |>
    fwa_join_stream_segments(elevation, source = source, zoom = zoom) |>
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
