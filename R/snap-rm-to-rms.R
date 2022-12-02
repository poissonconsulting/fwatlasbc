relocate_new_rm <- function (data) {
  data |>
    dplyr::relocate("new_rm", .after = "rm") |>
    dplyr::relocate("distance_to_new_rm", .after = "new_rm")
}

snap_zeros <- function(x, rms) {
  mouth <- x$..fwa_x_rm == 0
  if(any(mouth) && any(rms$rm == 0)) {
    x$..fwa_provided_new_rm[mouth] <- 0L
  }
  x
}

snap_rm_to_rms <- function(x, rms, interval, snap_zeros) {
  if(snap_zeros) {
    x <- snap_zeros(x, rms)
  }
  x <- x |>
    snap_rm_to_point(rms)
  provided <- !is.na(x$..fwa_provided_new_rm)
  x$rm[provided] <- x$..fwa_provided_new_rm[provided]
  x$rm <- round_any(x$rm, interval)
  x$rm[provided] <- x$..fwa_provided_new_rm[provided]
  x
}

#' Snap River Meter to River Meters
#'
#' Assigns closest river meter to river meters based on blue line keys.
#' If x already includes new_rm column then non-missing values are preserved.
#' The non-missing new_rm values must be ordered.
#' The snap_zeros argument overwrites any existing non-missing new_rm values
#' where rm is 0 for the same blk.
#'
#' The closest river meter is snapped to each rm by blk and missing
#' new_rm values are replaced with the corresponding rm value.
#' The new_rm values are then ordered by adjusting the values so that
#' firstly all previous values are not greater than each provided new_rm value
#' and then all subsequent values are not less than the previous value.
#' Next all runs of two or more identical new_rm values that do not include
#' a provided new_rm are interpolated between the previous and subsequent
#' new_rm values based on the original rm spacing.
#' Finally all generated new_rm values are rounded to the interval.
#'
#' @param x An sf object of spatial points with blk and rm columns and optional new_rm integer column.
#' @param rm An sf object of spatial point with blk and rm columns.
#' @param interval A whole number of the interval to round generated new_rm values to.
#' @param snap_zeros A flag specifying whether to set new_rm to 0 where rm is 0.
#' @return An updated version of x with integer columns blk, rm and new_rm and numeric column distance_to_new_rm.
#' @export
#' @examples
#' rm <- fwa_add_rms_to_blk(data.frame(blk = 356308001))
#' x <- rm[rm$rm %in% c(0, 2000, 5000, 6000, 7000),]
#' rm <- rm[rm$rm %in% c(1000, 3000, 4000, 8000, 9000, 10000),]
#' fwa_snap_rm_to_rms(x, rm)
fwa_snap_rm_to_rms <- function(x, rm, interval = 5, snap_zeros = FALSE) {
  chk::chk_s3_class(x, "sf")
  chk::chk_s3_class(rm, "sf")
  chk_whole_number(interval)
  chk_gt(interval)
  chk_flag(snap_zeros)

  check_names(x, c("blk", "rm"))
  check_names(rm, c("blk", "rm"))
  chk_not_subset(colnames(x), c("..fwa_id", "..fwa_blk", "..fwa_provided_new_rm", "..fwa_x_rm"))

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
    x <- x |>
      tidyplus::add_missing_column(
        new_rm = integer(0),
        distance_to_new_rm = numeric(0)) |>
      relocate_new_rm()

    return(x)
  }
  if(!nrow(rm)) {
    x <- x |>
      tidyplus::add_missing_column(
        new_rm = NA_integer_,
        distance_to_new_rm = NA_real_) |>
      relocate_new_rm()
    return(x)
  }

  rm <- same_crs(rm, x)

  rm <- rm |>
    dplyr::select(rm, "blk", "rm")

  x |>
    tidyplus::add_missing_column(new_rm = NA_integer_) |>
    dplyr::mutate(new_rm = as.integer(new_rm),
                  ..fwa_id = 1:dplyr::n()) |>
    dplyr::rename(..fwa_provided_new_rm = "new_rm",
                  ..fwa_x_rm = "rm") |>
    group_split_sf(.data$blk) |>
    lapply(snap_rm_to_rms, rm = rm, interval = interval, snap_zeros = snap_zeros) |>
    dplyr::bind_rows() |>
    dplyr::arrange(.data$..fwa_id) |>
    dplyr::rename(new_rm = "rm",
                  distance_to_new_rm = "distance_to_rm",
                  rm = "..fwa_x_rm") |>
    dplyr::mutate(
      blk = as.integer(.data$blk)) |>
    dplyr::select(!c("..fwa_id", "..fwa_blk", "..fwa_provided_new_rm")) |>
    relocate_new_rm()
}
