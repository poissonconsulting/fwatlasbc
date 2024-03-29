nearest_rm <- function(x, rm) {
  index <- sf::st_nearest_feature(x, rm)
  rm <- rm[index,]
  x$..fwa_blk <- rm$blk
  x$rm <- rm$rm
  x$distance_to_rm <- sf::st_distance(x, rm, by_element = TRUE)
  x$distance_to_rm <- as.numeric(x$distance_to_rm)
  x
}

adjust_rm <- function(rm, x) {
  if(ncol(rm) == 3) {
    return(rm)
  }

  cols <- rm |>
    colnames() |>
    setdiff(c("blk", "rm", "geometry"))

  cols <- x |>
    as_tibble() |>
    dplyr::select(tidyselect::vars_select_helpers$all_of(cols)) |>
    dplyr::slice(1L) |>
    dplyr::select(tidyselect::vars_select_helpers$where(function(x) !is.na(x[1]))) |>
    colnames()

  if(!length(cols)) {
    return(rm)
  }
  rm |>
    dplyr::semi_join(as_tibble(x), by = cols)
}

snap_rm_to_point <- function(x, rm) {
  if(!is.na(x$blk[1])) {
    rm <- rm[rm$blk == x$blk[1],]
  }
  rm <- adjust_rm(rm, x)

  if(!nrow(rm)) {
    x$rm <- NA_integer_
    x$distance_to_rm <- NA_real_
    return(x)
  }

  nearest_rm(x, rm)
}

#' Snap River Meter to Point
#'
#' Assigns closest river meter to each spatial point.
#' If the blue line key (blk) is missing then it is also assigned
#' together with the distance to the river meter (distance_to_rm) in m.
#'
#' @param x An sf object of spatial points with optional integer column blk.
#' @param rm An sf object of spatial point with blk and rm columns.
#' @param ... Additional columns to group by when assigning.
#' Elements with missing values in rm are assigned to any value in x.
#' @return An updated version of x with integer columns blk and rm and numeric column distance_to_rm.
#' @export
#' @examples
#' rm <- fwa_add_rms_to_blk(data.frame(blk = 356308001))
#' x <- rm[rm$rm %in% c(0, 2000, 5000, 6000, 7000),]
#' rm <- rm[rm$rm %in% c(1000, 3000, 4000, 8000, 9000, 10000),]
#' fwa_snap_rm_to_point(x, rm)
fwa_snap_rm_to_point <- function(x, rm, ...) {
  chk::chk_s3_class(x, "sf")
  chk::chk_s3_class(rm, "sf")

  if(!has_name(x, "blk")) x$blk <- NA_integer_

  chk_whole_numeric(x$blk)
  chk_gt(x$blk)
  chk_not_subset(colnames(x), c("..fwa_id", "..fwa_blk"))

  check_names(rm, c("blk", "rm"))

  chk_whole_numeric(rm$blk)
  chk_not_any_na(rm$blk)
  chk_gt(rm$blk)
  chk_whole_numeric(rm$rm)
  chk_not_any_na(rm$rm)
  chk_gte(rm$rm)

  if(!nrow(x)) {
    x$rm <- integer(0)
    x$distance_to_rm <- numeric(0)
    return(x)
  }
  if(!nrow(rm)) {
    x$rm <- NA_integer_
    x$distance_to_rm <- NA_real_
    return(x)
  }

  rm <- same_crs(rm, x)

  rm <- rm |>
    dplyr::select(rm, "blk", "rm", ...)

  x |>
    dplyr::mutate(..fwa_id = 1:dplyr::n()) |>
    group_split_sf(.data$blk, ...) |>
    lapply(snap_rm_to_point, rm = rm) |>
    dplyr::bind_rows() |>
    dplyr::arrange(.data$..fwa_id) |>
    dplyr::mutate(
      ..fwa_blk = as.integer(.data$..fwa_blk),
      blk = as.integer(.data$blk),
      blk = dplyr::if_else(is.na(.data$..fwa_blk), .data$blk, .data$..fwa_blk)) |>
    dplyr::relocate("distance_to_rm", .after = "rm") |>
    dplyr::select(!c("..fwa_id", "..fwa_blk"))
}
