convert_stream_segment_to_rms <- function(x, interval) {
  down <- x$downstream_route_measure
  up <- x$upstream_route_measure

  down_rm <- plyr::round_any(down, interval, ceiling)
  up_rm <- plyr::round_any(up, interval, floor)

  if(down_rm > up_rm) {
    rm <- integer(0)
    sample <- numeric(0)
  } else {
    rm <- seq(down_rm, up_rm, by = interval)
    rm <- as.integer(rm)
    sample <- (rm - down) / up
  }
  y <- x |>
    dplyr::as_tibble()

  if("geometry" %in% names(y))
    y$geometry <- NULL

  if("rm" %in% names(y))
    y$rm <- NULL

  x <- x |>
    sf::st_line_sample(sample = sample) |>
    sf::st_cast("POINT")

  dplyr::tibble(geometry = x, rm  = rm) |>
    sf::st_sf() |>
    dplyr::mutate(id = y$id) |>
    dplyr::left_join(y, by = "id") |>
    dplyr::relocate(.data$rm, .after = "id") |>
    dplyr::relocate(.data$geometry, .after = dplyr::last_col()) |>
    dplyr::arrange(.data$rm)
}

#' Convert Stream Network to River Meters
#'
#' @param x An sf tibble of a stream network.
#' @param interval A whole numeric of the distance between points.
#' @return An sf tibble with the columns of x plus integer column rm
#' and sf column geometry.
#' @family rm
#' @export
#' @examples
#' x <- fwa_add_stream_network_to_blk(data.frame(blk = 356308001))
#' fwa_convert_stream_network_to_rms(x)
fwa_convert_stream_network_to_rms <- function(x, interval = 5, tolerance = 0.1) {
  chk_s3_class(x, "sf")
  chk_whole_number(interval)
  chk_gt(interval)
  chk_number(tolerance)
  chk_gte(tolerance)

  check_names(x, c("id", "blue_line_key", "downstream_route_measure",
                   "upstream_route_measure"))
  chk_not_subset(colnames(x), "..fwa_id")

  chk_not_any_na(x$id)
  check_key(x, "id")

  chk_whole_numeric(x$blue_line_key)
  chk_not_any_na(x$blue_line_key)
  chk_gt(x$blue_line_key)

  chk_numeric(x$downstream_route_measure)
  chk_not_any_na(x$downstream_route_measure)
  chk_gte(x$downstream_route_measure)

  chk_numeric(x$upstream_route_measure)
  chk_not_any_na(x$upstream_route_measure)
  chk_gt(x$upstream_route_measure)

  diff <- x$upstream_route_measure - x$downstream_route_measure
  diff <- diff - as.numeric(sf::st_length(x$geometry))
  diff <- abs(diff)

  if(any(diff > tolerance)) {
    abort_chk("Difference between and upstream and down route measures and length of geometry exceeds tolerance in `x`")
  }

  x |>
    dplyr::mutate(..fwa_id = 1:dplyr::n()) |>
    dplyr::group_split(.data$..fwa_id) |>
    lapply(convert_stream_segment_to_rms, interval = interval) |>
    dplyr::bind_rows() |>
    dplyr::arrange(.data$..fwa_id) |>
    dplyr::select(-.data$..fwa_id)
}
