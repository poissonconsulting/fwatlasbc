#' Convert River Metres to Streams
#'
#' Casts river metre points to linestrings by blk.
#'
#' @param x An point sf object with whole numeric columns blk and rm.
#' @return A linestring sf object with whole numeric columns blk and rm.
#' @export
fwa_convert_rms_to_streams <- function(x) {
  chk::chk_s3_class(x, "sf")
  chk_s3_class(sf::st_geometry(x), "sfc_POINT")
  check_names(x, c("blk", "rm"))

  chk_whole_numeric(x$blk)
  chk_not_any_na(x$blk)
  chk_gt(x$blk)
  chk_whole_numeric(x$rm)
  chk_not_any_na(x$rm)
  chk_gte(x$rm)
  chk_whole_numeric(x$blk)

  x <- x |>
    dplyr::arrange(.data$blk, .data$rm) |>
    dplyr::group_by(.data$blk) |>
    dplyr::summarise(do_union = FALSE) |>
    sf::st_cast("LINESTRING")

  x
}
