is_rooted_blk <- function(x, blk) {
  identical(as.numeric(min(x$rm[x$blk == blk])), 0)
}

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

  root <- x[x$blk == blk & x$rm == 0,]
  check_dim(root, nrow, values = 1L) # +chk
  root$blk <- parent_blk
  root$rm <- parent_rm
  root$..fwa_trib <- FALSE

  x <- dplyr::bind_rows(x, root)

  is_main <- x$blk == parent_blk & x$rm >= parent_rm

  x$parent_rm[is_main] <- parent_rm
  x$parent_blk[is_main] <- parent_blk
  x$blk[is_main] <- blk
  x$rm[is_main] <- x$rm[is_main] - parent_rm
  x$..fwa_original[is_main] <- FALSE

  update_children_main(x, blk, parent_blk, parent_rm)
}

adjust_points_blk <- function(x, blk) {
  y <- x[x$blk == blk,]
  if(nrow(y) < 2) return(x)

  range <- range(y$rm)
  interval <- max(diff(sort(y$rm)))
  seq <- seq(0, range[2], by = interval)
  seq <- seq[seq >= range[1] & seq <= range[2]]

  if(!length(seq)) return(x)

  x <- x[x$blk != blk,]

  y <- y |>
    dplyr::arrange(rm)

  coordinates <- sf::st_coordinates(y) |>
    dplyr::as_tibble() |>
    mutate(rm = y$rm)

  new_y <- y |>
    dplyr::slice(1L:length(seq)) |>
    dplyr::mutate(rm = seq)

  X <- approx(coordinates$rm, coordinates$X, new_y$rm)$y
  Y <- approx(coordinates$rm, coordinates$Y, new_y$rm)$y

  sfc <- matrix(c(X, Y), ncol = 2) |>
    sf::st_multipoint(dim = "XY") |>
    sf::st_sfc(crs = sf::st_crs(y)) |>
    sf::st_cast("POINT")

  new_y <- new_y |>
    sf::st_set_geometry(sfc)

  x |>
    dplyr::bind_rows(new_y)
}

adjust_points <- function(x, blk, adjust_points) {
  if(!adjust_points) return(x)
  parent_blk <- parent_blk(x, blk, TRUE)
  x |>
    adjust_points_blk(blk) |>
    adjust_points_blk(parent_blk)
}

swap_branches <- function(x, blk, adjust_points) {
  chk_whole_number(blk) # +chk

  if(!is_rooted_blk(x, blk)) {
    abort_chk("`blk` ", blk, " from `x` is unrooted (missing rm == 0)")
  }

  x <- x |>
    fwa_add_upstream_split_to_rms(data.frame(blk = blk, rm = 0, name = "..fwa_trib")) |>
    dplyr::mutate(..fwa_original = TRUE) |>
    swap_main(blk) |>
    swap_trib(blk) |>
    adjust_points(blk, adjust_points) |>
    dplyr::mutate(rm = round_up(.data$rm)) |>
    dplyr::arrange(dplyr::desc(.data$..fwa_original)) |>
    dplyr::distinct(.data$blk, .data$rm, .keep_all = TRUE) |>
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
  chk_not_subset(colnames(x), c("..fwa_trib", "..fwa_original"))
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

  check_key(x, c("blk", "rm"))

  if(!nrow(y)) return(x)

  missing <- setdiff(y$blk, x$blk)
  if(length(missing)) {
    abort_chk("The following %n blk%s %r missing from `x`:", cc(missing, conj = "and"),
              n = length(missing))
  }

  for(blk in y$blk) {
    x <- swap_branches(x, blk, adjust_points)
  }
  x |>
    dplyr::arrange(.data$blk, .data$rm)
}
