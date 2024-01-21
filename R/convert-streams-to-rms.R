get_parent_blk_rm <- function(x, y, gap) {

  y <- y |>
    dplyr::anti_join(as_tibble(x), by = "blk")

  x <- x |>
    dplyr::mutate(parent_blk = sf::st_nearest_feature(x, y),
                  parent_blk = y$blk[.data$parent_blk]) |>
    dplyr::left_join(as_tibble(y), by = c("parent_blk" = "blk"))

  span <- sf::st_nearest_points(x$geometry, x$..fwa_linestring, pairwise = TRUE)
  distance <- sf::st_length(span) |> as.numeric()

  if(distance > gap) {
    x$parent_blk <- NA_integer_
    x$parent_rm <- NA_real_
  } else {
    point <- sf::st_cast(span, "POINT") |>
      dplyr::nth(2)

    if(distance > 0) {
      point <- sf::st_buffer(point, distance)
    }

    parent_rm <- lwgeom::st_split(x$..fwa_linestring, point) |>
      sf::st_collection_extract("LINESTRING")

    start <- sf::st_line_sample(x$..fwa_linestring, sample = 0)

    intersects <- sf::st_intersects(parent_rm, start, sparse = FALSE)

    parent_rm <- parent_rm |>
      dplyr::nth(which(intersects)) |>
      sf::st_length() |>
      as.numeric()

    x$parent_rm <- parent_rm
  }
  x |>
    as_tibble() |>
    dplyr::select("blk", "parent_blk", "parent_rm")
}

get_parent_stream <- function(x, y, gap) {
  sf::st_geometry(y) <- "..fwa_linestring"

  mouth <- x |>
    dplyr::filter(rm == 0) |>
    dplyr::group_split(.data$blk) |>
    purrr::map(get_parent_blk_rm, y, gap) |>
    dplyr::bind_rows() |>
    as_tibble() |>
    dplyr::select("blk", "parent_blk", "parent_rm")

  x |>
    left_join(mouth, by = "blk")
}

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
#' @param end A positive whole number indicating how far (m) the end of
#' the stream linestring has to be from the last interval to be included.
#' To  default `end = NULL` (equivalent to `end = interval + 1`) excludes ends.
#' @param elevation A flag specifying whether to use the elevation
#' from Google Maps to determine stream direction (or use the
#' direction of the provided linestrings)
#' @param reverse A whole numeric vector of streams to reverse direction
#' ignoring elevation.
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
fwa_convert_streams_to_rms <- function(x, interval = 5, gap = 1, end = NULL, elevation = FALSE,
                                       reverse = integer()) {
  chk_s3_class(x, "sf")
  chk_whole_number(interval)
  chk_gt(interval)

  check_names(x, "blk")
  chk_not_subset(colnames(x), "..fwa_length")

  chk_whole_numeric(x$blk)
  chk_not_any_na(x$blk)
  chk_gt(x$blk)

  chk_null_or(end, vld = vld_whole_number)
  if(is.null(end)) {
    end <- interval + 1
  }
  chk_gt(end)

  chk_whole_numeric(reverse)
  chk_not_any_na(reverse)
  chk_gt(reverse)
  chk_unique(reverse)
  chk_subset(reverse, x$blk)

  crs <- sf::st_crs(x)

  x <- x |>
    fwa_join_stream_segments(elevation, reverse)

  x |>
    sample_linestrings(interval, end = end) |>
    get_parent_stream(x, gap = gap) |>
    sf::st_set_crs(crs)
}
