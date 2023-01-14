#' Add New Blue Line Key and River Meter to Blue Line Key and River Meter
#'
#' Adds new blk and rm values where existing blk and rm values
#' based on look up in rm.
#'
#' @param x A tibble (or sf object) with blk and rm columns.
#' @param rm A tibble (or sf object) with unique blk and rm columns
#' and new blk and rm columns.
#' @return An updated version of x with
#' additional new_blk and new_rm columns.
#' @export
fwa_add_new_blk_rm_to_blk_rm <- function(x, rm) {
  check_names(x, c("blk", "rm"))
  chk_not_subset(colnames(x), c("new_blk", "new_rm"))

  chk_whole_numeric(x$blk)
  chk_gt(x$blk)

  chk_whole_numeric(x$rm)
  chk_gte(x$rm)

  check_names(rm, c("blk", "rm", "new_blk", "new_rm"))
  chk_whole_numeric(rm$blk)
  chk_not_any_na(rm$blk)
  chk_gt(rm$blk)

  chk_whole_numeric(rm$rm)
  chk_not_any_na(rm$rm)
  chk_gte(rm$rm)

  check_key(rm, c("blk", "rm"))

  chk_whole_numeric(rm$new_blk)
  chk_gt(rm$new_blk)

  chk_whole_numeric(rm$new_rm)
  chk_gte(rm$new_rm)

  rm <- rm |>
    dplyr::as_tibble() |>
    dplyr::select("blk", "rm", "new_blk", "new_rm")

  x |>
    dplyr::left_join(rm, by = c("blk", "rm")) |>
    dplyr::relocate() |>
    dplyr::relocate("new_blk", "new_rm", .after = "rm")
}
