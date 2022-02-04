#' Get Section from River Meter
#'
#' Gets sf tibble of section blk, rm, length and geometry
#' where sections are defined by the their most upstream
#' point.
#'
#' @param x A data frame with integer columns blk and rm.
#' @param section A string of the name of the column in x that specifies the sections.
#' @return A sf tibble with integer columns blk, rm, length and a geometry where rm and geometry are the upstream end of the section.
#' @export
fwa_get_section_from_rms <- function(x, section = "section") {
  chk_data(x)
  chk_string(section)
  chk_not_subset(section, c("blk", "rm"))
  check_names(x, c("blk", "rm", section))

  chk_whole_numeric(x$blk)
  chk_not_any_na(x$blk)
  chk_gt(x$blk)
  chk_whole_numeric(x$rm)
  chk_not_any_na(x$rm)
  chk_gte(x$rm)
  chk_whole_numeric(x$blk)

  check_key(x, c("blk", "rm"))

  sf_column_name <- sf_column_name(x)
  stopifnot(sf_column_name != section)

  y <- x |>
    as_tibble() |>
    dplyr::filter(!is.na(.data[[section]])) |>
    dplyr::group_by(.data$blk, .data[[section]]) |>
    dplyr::summarise(length = max(.data$rm) - min(.data$rm),
                     rm = max(.data$rm)) |>
    dplyr::ungroup() |>
    dplyr::select(.data[[section]], .data$blk, .data$rm, .data$length) |>
    dplyr::arrange(.data$blk, .data$rm)

  if(!identical(sf_column_name, character(0))) {
    x <- x |>
      dplyr::select(.data$blk, .data$rm)
     y <- y |>
       dplyr::inner_join(x, by = c("blk", "rm")) |>
       sf::st_sf(sf_column_name = sf_column_name)
  }
  y
}
