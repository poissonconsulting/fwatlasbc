#' Get Segment from River Meter
#'
#' Gets sf tibble of section blk, rm_start, rm_end, segment and line geometry.
#'
#' @param x A data frame with integer columns blk and rm.
#' @param segment A string of the name of the column in x, which must be a character or factor vector, that specifies which segment each rms belongs to.
#' @return A sf tibble with integer columns blk, rm_start, rm_end, character/factor segment and a line geometry.
#' @export
fwa_get_segment_from_rms <- function(x, segment = "segment") {
  chk_data(x)
  chk_string(segment)
  chk_not_subset(segment, c("blk", "rm"))
  check_names(x, c("blk", "rm", segment))

  chk_whole_numeric(x$blk)
  chk_not_any_na(x$blk)
  chk_gt(x$blk)
  chk_whole_numeric(x$rm)
  chk_not_any_na(x$rm)
  chk_gte(x$rm)
  chk_whole_numeric(x$blk)
  chk_character_or_factor(x[[segment]])

  check_key(x, c("blk", "rm"))

  sf_column_name <- sf_column_name(x)
  stopifnot(sf_column_name != segment)

  x |>
    dplyr::filter(!is.na(.data[[segment]])) |>
    dplyr::group_by(.data[[segment]], .data$blk) |>
    dplyr::arrange(.data$rm) |>
    dplyr::summarise(rm_start = min(.data$rm), rm_end = max(.data$rm), do_union = FALSE) |>
    sf::st_cast("LINESTRING") |>
    dplyr::arrange(.data$blk, .data$rm_start)
}
