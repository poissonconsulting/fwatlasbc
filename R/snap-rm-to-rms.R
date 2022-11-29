relocate_new_rm <- function (data) {
  data |>
    dplyr::relocate("new_rm", .after = "rm") |>
    dplyr::relocate("distance_to_new_rm", .after = "new_rm")
}

#' Snap River Meter to River Meters
#'
#' Assigns closest river meter to river meters based on blue line keys.
#'
#' @param x An sf object of spatial points with blk and rm columns.
#' @param rm An sf object of spatial point with blk and rm columns.
#' @return An updated version of x with integer columns blk, rm and new_rm and numeric column distance_to_new_rm.
#' @export
#' @examples
#' rm <- fwa_add_rms_to_blk(data.frame(blk = 356308001))
#' x <- rm[rm$rm %in% c(0, 2000, 5000, 6000, 7000),]
#' rm <- rm[rm$rm %in% c(1000, 3000, 4000, 8000, 9000, 10000),]
#' fwa_snap_rm_to_rms(x, rm)
fwa_snap_rm_to_rms <- function(x, rm) {
  chk::chk_s3_class(x, "sf")
  chk::chk_s3_class(rm, "sf")

  check_names(x, c("blk", "rm"))
  check_names(rm, c("blk", "rm"))
  chk_not_subset(colnames(x), c("..fwa_id", "..fwa_blk"))

  chk_whole_numeric(x$blk)
  chk_not_any_na(x$blk)
  chk_gt(x$blk)

  chk_whole_numeric(x$rm)
  chk_not_any_na(x$rm)
  chk_gte(x$rm)

  chk_whole_numeric(rm$blk)
  chk_not_any_na(rm$blk)
  chk_gt(rm$blk)

  chk_whole_numeric(rm$rm)
  chk_not_any_na(rm$rm)
  chk_gte(rm$rm)

  if(!nrow(x)) {
    x$new_rm <- integer(0)
    x$distance_to_new_rm <- numeric(0)
    x <- x |>
      relocate_new_rm()

    return(x)
  }
  if(!nrow(rm)) {
    x$new_rm <- NA_integer_
    x$distance_to_new_rm <- NA_real_
    x <- x |>
      relocate_new_rm()

    return(x)
  }

  rm <- same_crs(rm, x)

  rm <- rm |>
    dplyr::select(rm, "blk", "rm")

  x |>
    dplyr::mutate(..fwa_id = 1:dplyr::n()) |>
    dplyr::rename(old_rm = "rm") |>
    group_split_sf(.data$blk) |>
    lapply(snap_rm_to_point, rm = rm) |>
    dplyr::bind_rows() |>
    dplyr::arrange(.data$..fwa_id) |>
    dplyr::rename(new_rm = "rm",
                  distance_to_new_rm = "distance_to_rm",
                  rm = "old_rm") |>
    dplyr::mutate(
      blk = as.integer(.data$blk)) |>
    dplyr::select(!c("..fwa_id", "..fwa_blk")) |>
    relocate_new_rm()
}
