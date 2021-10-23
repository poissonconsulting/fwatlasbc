parent_blk <- function(x, blk, defined = FALSE, trib = NA) {
  chk_whole_number(blk) # +chk

  trib <- if(is.na(trib)) rep(TRUE, nrow(x)) else x$..fwa_trib == trib
  x <- unique(x$parent_blk[x$blk == blk & trib])

  if(!length(x)) {
    abort_chk("`blk` ", blk, " is missing from `x`")
  }

  if(length(x) > 1) {
    abort_chk("`parent_blk` is inconsistent for blk ", blk, " in `x`")
  }
  if(defined & is.na(x)) {
    abort_chk("`parent_blk` is undefined for blk ", blk, " in `x`")
  }
  x
}

parent_rm <- function(x, blk, defined = FALSE, trib = NA) {
  chk_whole_number(blk) # +chk

  trib <- if(is.na(trib)) rep(TRUE, nrow(x)) else x$..fwa_trib == trib
  x <- unique(x$parent_rm[x$blk == blk & trib])

  if(!length(x)) {
    abort_chk("`blk` ", blk, " is missing from `x`")
  }

  if(length(x) > 1) {
    abort_chk("`parent_rm` is inconsistent for blk ", blk, " in `x`")
  }
  if(defined & is.na(x)) {
    abort_chk("`parent_rm` is undefined for blk ", blk, " in `x`")
  }
  x
}

update_children_trib <- function(x, blk, parent_blk, parent_rm) {
  is_child <- !is.na(x$parent_blk) & x$parent_blk == blk & x$..fwa_trib

  x$parent_blk[is_child] <- parent_blk
  x$parent_rm[is_child] <- x$parent_rm[is_child] + parent_rm
  x
}

update_children_main <- function(x, blk, parent_blk, parent_rm) {
  is_main <- x$blk != blk & !is.na(x$parent_blk) & x$parent_blk == parent_blk & !is.na(x$parent_rm) & x$parent_rm >= parent_rm

  x$parent_blk[is_main] <- blk
  x$parent_rm[is_main] <- x$parent_rm[is_main] - parent_rm
  x
}

swap_trib <- function(x, blk) {
  parent_blk <- parent_blk(x, blk, TRUE, trib = TRUE)
  parent_rm <- parent_rm(x, blk, TRUE, trib = TRUE)

  is_trib <- x$blk == blk & x$..fwa_trib

  x$parent_rm[is_trib] <- parent_rm(x, parent_blk)
  x$parent_blk[is_trib] <- parent_blk(x, parent_blk)
  x$blk[is_trib] <- parent_blk
  x$rm[is_trib] <- x$rm[is_trib] + parent_rm
  x$..fwa_original[is_trib] <- FALSE

  update_children_trib(x, blk, parent_blk, parent_rm)
}

swap_main <- function(x, blk) {
  parent_blk <- parent_blk(x, blk, TRUE)
  parent_rm <- parent_rm(x, blk, TRUE)

  is_main <- x$blk == parent_blk & x$rm >= parent_rm

  x$parent_rm[is_main] <- parent_rm
  x$parent_blk[is_main] <- parent_blk
  x$blk[is_main] <- blk
  x$rm[is_main] <- x$rm[is_main] - parent_rm
  x$..fwa_original[is_main] <- FALSE

  update_children_main(x, blk, parent_blk, parent_rm)
}

# adjust_main <- function(blk) {
#   is_new_main <- x$blk == blk & (x$rm == 0 | !..fwa_original)
#   parent_rm <- parent_rm(x, blk, TRUE)
#   current_rm <-
#
# }

swap_branches <- function(x, blk, adjust_points) {
  chk_whole_number(blk) # +chk

  x <- x |>
    fwa_add_split_to_rms(data.frame(blk = blk, rm = 0, name = "..fwa_trib")) |>
    dplyr::mutate(..fwa_original = TRUE) |>
    swap_main(blk) |>
#    adjust_main(blk, adjust_points) |>
    swap_trib(blk) |>
    dplyr::mutate(rm = round_up(.data$rm)) |>
    dplyr::arrange(dplyr::desc(.data$..fwa_original)) |>
    dplyr::distinct(.data$blk, .data$rm, .keep_all = TRUE) |>
    dplyr::arrange(.data$..fwa_index) |>
    dplyr::select(-.data$..fwa_trib, -.data$..fwa_original)
}

#' Swap Branches of River Meters
#'
#' Swaps two branches of river meters.
#'
#' @param x A data frame with integer columns blk, rm, parent_blk and parent_rm.
#' @param y A data frame with integer column blk specifying the
#' blue line key that currently begins at the confluence of the two branches.
#' @param adjust_points A flag specifying whether to adjust the coordinates of points which move.
#' @return A copy of x with the branches swapped.
#' @export
fwa_swap_branches_rms <- function(x, y, adjust_points = TRUE) {
  chk_data(x)
  chk_data(y)
  chk_flag(adjust_points)
  if(!(vld_false(adjust_points) | vld_s3_class(x, "sf"))) {
    chkor_vld(vld_false(adjust_points), vld_s3_class(x, "sf"))
  }

  if(vld_s3_class(x, "sf")) {
    chk_s3_class(sf::st_geometry(x), "sfc_POINT")
  }

  check_names(x, c("blk", "rm", "parent_blk", "parent_rm"))
  chk_not_subset(colnames(x), c("..fwa_index", "..fwa_trib", "..fwa_original"))
  check_names(y, "blk")

  chk_whole_numeric(x$blk)
  chk_not_any_na(x$blk)
  chk_gt(x$blk)

  chk_whole_numeric(x$rm)
  chk_not_any_na(x$rm)
  chk_gte(x$rm)

  chk_whole_numeric(x$parent_blk)
  chk_gt(x$parent_blk)

  chk_numeric(x$parent_rm)
  chk_gte(x$parent_rm)

  chk_whole_numeric(y$blk)
  chk_not_any_na(y$blk)
  chk_unique(y$blk)
  chk_gt(y$blk)

  if(!nrow(y)) return(x)

  missing <- setdiff(y$blk, x$blk)
  if(length(missing)) {
    abort_chk("The following %n blk%s %r missing from `x`:", cc(missing, conj = "and"),
              n = length(missing))
  }

  x <- x |>
    dplyr::mutate(..fwa_index = 1:n())

  for(blk in y$blk) {
    x <- swap_branches(x, blk, adjust_points)
  }
  x |>
    dplyr::arrange(.data$..fwa_index) |>
    dplyr::select(-.data$..fwa_index)
}
