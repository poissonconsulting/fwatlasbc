merge_blocks <- function(df) {
  if (nrow(df) == 1) return(df)
  df$delete <- FALSE
  for (i in 2:nrow(df)) {
    if (df$values[i] == df$values[i - 1]) {
      df$start[i] <- df$start[i - 1]
      df$length[i] <- df$length[i] + df$length[i - 1]
      df$delete[i - 1] <- TRUE
    }
  }
  df <- df[!df$delete, ]
  df$delete <- NULL
  df <- df[df$length > 1, ]
  df
}

select_closest <- function(rm, prev_new_rm) {
  chk_whole_number(prev_new_rm)
  rm$..fwa_keep[rm$new_rm < prev_new_rm] <- FALSE
  if (!any(rm$..fwa_keep)) return(rm)

  rm$..fwa_distance_to_rm <- sf::st_distance(rm, rm$..fwa_geometry, by_element = TRUE)
  rm$..fwa_distance_to_rm <- as.numeric(rm$..fwa_distance_to_rm)
  wch_keep <- which.min(rm$..fwa_distance_to_rm[rm$..fwa_keep])
  rm$..fwa_distance_to_rm <- NULL

  rm$..fwa_keep[rm$..fwa_keep][-wch_keep] <- FALSE
  rm
}

resolve_multijoins <- function(rm) {
  rle <- rle(rm$rm)

  df <- data.frame(
    values = rle$values,
    length = rle$lengths
  )
  df$end <- cumsum(df$length)
  df$start <- df$end - df$length + 1

  df <- df[!is.na(df$values), ]
  if (!nrow(df)) return(rm)

  df <- merge_blocks(df)
  if (!nrow(df)) return(rm)

  prev_new_rm <- 0
  rm$..fwa_keep <- TRUE
  for (i in 1:nrow(df)) {
    indices <- df$start[i]:df$end[i]
    rm[indices, ] <- select_closest(rm[indices, ], prev_new_rm)
    prev_new_rm <- rm$new_rm[indices][rm$..fwa_keep[indices]]
  }
  rm <- rm[rm$..fwa_keep, ]
  rm$..fwa_keep <- NULL
  rm
}

#' Snap River Meters to River Meters
#'
#' Assigns closest river meters to river meters by blue line keys using
#' `fwa_snap_rm_to_rms()`
#' rm must not have an existing new_rm column.
#'
#' x is first snapped to rm then rm is snapped to x while ensuring
#' that the links between x and rm are bidirectional as much as possible.
#'
#' @param x An sf object of spatial points with blk and rm columns and
#' optional new_blk and new_rm integer column.
#' @param rm An sf object of spatial point with blk and rm columns.
#' @param snap_mouths A flag specifying whether to snap pairs of streams at their mouths (rm = 0) where new_rm is not already set.
#' @return A named list with an updated versions of x and rm with
#' integer columns blk, new_blk, rm and new_rm and
#' numeric column distance_to_new_rm.
#' @seealso [fwa_snap_rm_to_rms()]
#' @export
#' @examples
#' rm <- fwa_add_rms_to_blk(data.frame(blk = 356308001))
#' x <- rm[rm$rm %in% c(0, 2000, 5000, 6000, 7000), ]
#' rm <- rm[rm$rm %in% c(1000, 3000, 4000, 8000, 9000, 10000), ]
#' fwa_snap_rms_to_rms(x, rm)
fwa_snap_rms_to_rms <- function(x, rm, snap_mouths = FALSE) {
  chk::chk_s3_class(rm, "sf")

  check_names(rm, c("blk", "rm"))
  chk_not_subset(colnames(rm), "new_rm")

  chk_whole_numeric(x$rm)
  chk_not_any_na(x$rm)
  chk_gte(x$rm)
  check_key(x, c("blk", "rm"))

  chk_whole_numeric(rm$blk)
  chk_not_any_na(rm$blk)
  chk_gt(rm$blk)

  x <- fwa_snap_rm_to_rms(x, rm, snap_mouths = snap_mouths)
  geometry <- sf::st_geometry(x)
  x2 <- x |>
    as_tibble()
  x2 <- x2[c("blk", "new_rm", "rm")]
  x2$rm2 <- x2$rm
  x2$rm <- x2$new_rm
  x2$new_rm <- x2$rm2
  x2$rm2 <- NULL
  x2$..fwa_geometry <- geometry

  rm <- rm |>
    dplyr::left_join(x2, by = c("blk", "rm"), multiple = "all")

  rm <- rm |>
    group_split_sf(.data$blk) |>
    lapply(resolve_multijoins) |>
    dplyr::bind_rows()

  rm$..fwa_geometry <- NULL

  rm <- rm |>
    fwa_snap_rm_to_rms(x, snap_mouths = snap_mouths)

  list(x = x, rm = rm)
}
