strip_trailing_0s <- function(x) {
  str_replace_all(x, "-0{6,6}", "")
}

get_parent_code <- function(x) {
  str_replace(x, "-\\d{6,6}$", "")
}

get_parent_proportion <- function(x) {
  x |>
    str_extract("\\d{6,6}$") |>
    as.integer() |>
    divide_by(1000000)
}

add_parent_blk_rm <- function(x) {
  parent <- x |>
    dplyr::as_tibble() |>
    dplyr::filter(!str_detect(.data$fwa_watershed_code, "^999")) |>
    dplyr::mutate(fwa_watershed_code = strip_trailing_0s(.data$fwa_watershed_code)) |>
    dplyr::group_by(.data$blue_line_key, .data$fwa_watershed_code) |>
    dplyr::summarise(min_rm = min(.data$upstream_route_measure),
                     max_rm = max(.data$upstream_route_measure)) |>
    dplyr::ungroup() |>
    dplyr::filter(.data$min_rm != 0)

  check_key(parent, "blue_line_key")

  # drops main parents as multiple blks!
  parent <- parent |>
    dplyr::filter(!str_detect(.data$fwa_watershed_code, "^\\d{3,3}$"))

  child <- parent |>
    dplyr::mutate(
      parent_code = get_parent_code(.data$fwa_watershed_code),
      parent_proportion = get_parent_proportion(.data$fwa_watershed_code)) |>
    dplyr::select(
      child_blk = .data$blue_line_key, .data$parent_code, .data$parent_proportion) |>
    dplyr::inner_join(parent, by = c(parent_code = "fwa_watershed_code")) |>
    dplyr::mutate(parent_rm = .data$parent_proportion * .data$max_rm) |>
    dplyr::select(blue_line_key = .data$child_blk, parent_blk = .data$blue_line_key, .data$parent_rm)

  x |>
    dplyr::left_join(child, by = "blue_line_key") |>
    dplyr::relocate(.data$parent_blk, .data$parent_rm, .after = "id")
}

convert_stream_segment_to_rms <- function(x, interval) {
  down <- x$downstream_route_measure
  up <- x$upstream_route_measure

  down_rm <- round_any(down, interval, ceiling)
  up_rm <- round_any(up, interval, floor)

  if(down_rm > up_rm) {
    rm <- integer(0)
    sample <- numeric(0)
  } else {
    rm <- seq(down_rm, up_rm, by = interval)
    rm <- as.integer(rm)
    sample <- (rm - down) / (up - down)
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
    dplyr::mutate(blk = .data$blue_line_key) |>
    dplyr::relocate(.data$blk, .data$rm, .after = "id") |>
    dplyr::relocate(.data$geometry, .after = dplyr::last_col())
}

#' Convert Stream Network to River Meters
#'
#' @param x An sf tibble of a stream network.
#' @param interval A whole number of the distance between points.
#' @param tolerance A number of the acceptable
#' discrepancy in meters in the network lengths.
#' @return An sf tibble with the columns of x plus integer column rm
#' and sf column geometry.
#' @export
#' @examples
#' \dontrun{
#' watershed <- fwa_add_watershed_to_blk(data.frame(blk = 356308001, rm = 1000))
#' network <- fwa_add_collection_to_polygon(watershed)
#' fwa_convert_stream_network_to_rms(network, interval = 100)
#' }
fwa_convert_stream_network_to_rms <- function(x, interval = 5, tolerance = 0.1) {
  chk_s3_class(x, "sf")
  chk_whole_number(interval)
  chk_gt(interval)
  chk_number(tolerance)
  chk_gte(tolerance)

  check_names(x, c("id", "blue_line_key", "downstream_route_measure",
                   "upstream_route_measure", "fwa_watershed_code"))
  chk_not_subset(colnames(x), "..fwa_id")

  chk_not_any_na(x$id)
  check_key(x, "id")

  chk_whole_numeric(x$blue_line_key)
  chk_not_any_na(x$blue_line_key)
  chk_gt(x$blue_line_key)

  chk_character_or_factor(x$fwa_watershed_code)
  chk_not_any_na(x$fwa_watershed_code)
  chk_match(x$fwa_watershed_code, "^\\d{3,3}(-\\d{6,6})+-(0{6,6}|9{6,6})$")

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
    add_parent_blk_rm() |>
    dplyr::mutate(..fwa_id = 1:dplyr::n()) |>
    dplyr::group_split(.data$..fwa_id) |>
    lapply(convert_stream_segment_to_rms, interval = interval) |>
    dplyr::bind_rows() |>
    dplyr::arrange(.data$blk, .data$rm) |>
    dplyr::select(-.data$..fwa_id)
}
