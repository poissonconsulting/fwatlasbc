nearest_rm <- function(x, rm) {
  index <- sf::st_nearest_feature(x, rm)
  rm <- rm[index,]
  x$..fwa_BLK <- rm$BLK
  x$RM <- rm$RM
  x$Distance <- sf::st_distance(x, rm, by_element = TRUE)
  x$Distance <- as.numeric(x$Distance)
  x
}

snap_rm_to_point <- function(x, rm) {
  if(!is.na(x$BLK[1])) {
    rm <- rm[rm$BLK == x$BLK[1],]
  }
  nearest_rm(x, rm)
}

#' Snap River Meter to Point
#'
#' Assigns closest river meter to each spatial point.
#' If the blue line key (BLK) is missing then it is also assigned
#' together with the distance (in m).
#'
#' @param x An sf object of spatial points with optional integer column BLK.
#' @param rm An sf object of spatial point with BLK and RM columns.
#' @return An updated version of x with integer columns BLK and RM and numeric column Distance.
#' @family rm
#' @export
#' @examples
#' \dontrun{
#' rm <- fwa_rm(blue_line_key = 356308001, interval = 1000)
#' fwa_snap_rm_to_point(rm[2:4,], rm[3:5,])
#' }
fwa_snap_rm_to_point <- function(x, rm) {
  chk::chk_s3_class(x, "sf")
  chk::chk_s3_class(rm, "sf")

  if(is.null(x$BLK)) x$BLK <- NA_integer_

  chk_whole_numeric(x$BLK)
  chk_gt(x$BLK)
  chk_not_subset(colnames(x), c("..fwa_id", "..fwa_BLK"))

  check_names(rm, c("BLK", "RM"))

  chk_whole_numeric(rm$BLK)
  chk_not_any_na(rm$BLK)
  chk_gt(rm$BLK)
  chk_whole_numeric(rm$RM)
  chk_not_any_na(rm$RM)
  chk_gte(rm$RM)

  if(!nrow(x)) {
    x$RM <- integer(0)
    x$Distance <- numeric(0)
    return(x)
  }
  if(!nrow(rm)) {
    x$RM <- NA_integer_
    x$Distance <- NA_real_
    return(x)
  }

  x |>
    dplyr::mutate(..fwa_id = 1:dplyr::n()) |>
    dplyr::group_split(.data$BLK) |>
    lapply(snap_rm_to_point, rm = rm) |>
    dplyr::bind_rows() |>
    dplyr::arrange(.data$..fwa_id) |>
    dplyr::mutate(BLK = .data$..fwa_BLK) |>
    dplyr::relocate(.data$Distance, .after = "RM") |>
    dplyr::select(-.data$..fwa_id, -.data$..fwa_BLK)
}
