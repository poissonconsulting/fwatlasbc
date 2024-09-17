#' Add New Blue Line Key and River Meter to Blue Line Key and River Meter
#'
#' Adds new blk and rm values to existing blk and rm values
#' based on look up in second data frame.
#'
#' @param x A tibble (or sf object) with blk and rm columns.
#' @param y A tibble (or sf object) with unique blk and rm columns
#' and new blk (optional) and rm columns.
#' @param blk A string of the name of the blk column in x.
#' @param rm A string of the name of the rm column in x.
#' @param blk2 A string of the name of the blk column in y.
#' @param rm2 A string of the name of the rm column in y.
#' @param new_blk An optional string of the name of the new blk column in y.
#' If NULL it is not used in the join.
#' @param new_rm A string of the name of the new rm column in y.
#' @param new_blk_to A string specifying the name to rename
#' the new blk column with.
#' @param new_rm_to A string specifying the name to rename
#' the new blk column with.
#' @return An updated version of x with
#' additional columns for the new blk and rm columns.
#' @seealso `[fwa_add_new_rm_to_blk_rm()]`
#' @export
fwa_add_new_blk_rm_to_blk_rm <- function(
    x, y,
    blk = "blk", rm = "rm",
    blk2 = "blk", rm2 = "rm",
    new_blk = "new_blk",
    new_rm = "new_rm",
    new_blk_to = new_blk,
    new_rm_to = new_rm) {
  chk_data(x)
  chk_data(y)

  chk_string(blk)
  chk_string(rm)
  chk_string(blk2)
  chk_string(rm2)
  chk_null_or(new_blk, vld = vld_string)
  chk_string(new_rm)
  chk_null_or(new_blk_to, vld = vld_string)
  chk_string(new_rm_to)

  if (vld_string(new_blk) && new_blk == blk2) {
    new_blk <- NULL
    new_blk_to <- NULL
  }

  chk_unique(c(blk, rm, new_blk, new_rm))
  chk_unique(c(blk, rm, new_blk_to, new_rm_to))
  chk_unique(c(blk2, rm2, new_blk, new_rm))

  check_names(x, c(blk, rm))
  chk_not_subset(colnames(x), c(new_blk_to, new_rm_to))
  check_names(y, c(blk2, rm2, new_blk, new_rm))

  chk_whole_numeric(x[[blk]])
  chk_gt(x[[blk]])

  chk_whole_numeric(x[[rm]])
  chk_gte(x[[rm]])

  chk_whole_numeric(y[[blk2]])
  chk_not_any_na(y[[blk2]])
  chk_gt(y[[blk2]])

  chk_whole_numeric(y[[rm2]])
  chk_not_any_na(y[[rm2]])
  chk_gte(y[[rm2]])

  check_key(y, c(blk2, rm2))

  if (!is.null(new_blk)) {
    chk_whole_numeric(y[[new_blk]])
    chk_gt(y[[new_blk]])
  }

  chk_whole_numeric(y[[new_rm]])
  chk_gte(y[[new_rm]])

  y <- dplyr::as_tibble(y) |>
    dplyr::select(all_of(c(blk2, rm2, new_blk, new_rm)))

  colnames(y) <- c(blk2, rm2, new_blk_to, new_rm_to)

  join <- c(blk = blk2, rm = rm2)
  names(join) <- c(blk, rm)

  x |>
    dplyr::left_join(y, by = join) |>
    dplyr::relocate(all_of(c(new_blk_to, new_rm_to)), .after = all_of(rm))
}
