new_blk_to_blk <- function(x) {
  x |>
    dplyr::select(!"blk") |>
    dplyr::rename(blk = "new_blk")
}

relocate_blks_new_rm <- function (data) {
  data |>
    dplyr::relocate("blk", "new_blk") |>
    dplyr::relocate("new_rm", .after = "rm") |>
    dplyr::relocate("distance_to_new_rm", .after = "new_rm")
}

distance_to_rm <- function(x, rms) {
  fac <- factor(rms$rm, levels = rms$rm)
  rm <- rms[as.integer(factor(x$rm, levels = levels(fac))),]
  x$distance_to_rm <- sf::st_distance(x, rm, by_element = TRUE)
  x$distance_to_rm <- as.numeric(x$distance_to_rm)
  x
}

prev_cummax <- function(x) {
  if(!length(x)) return(x)
  c(x[1], cummax(x)[-length(x)])
}

interpolate_block <- function(x, start, end) {
  ax <- c(x$..fwa_x_rm[start-1], x$..fwa_x_rm[end+1])
  ay <- c(x$rm[start-1], x$rm[end+1])
  indices <- start:end
  xout <- x$..fwa_x_rm[indices]

  x$rm[indices] <- approx(ax, ay, xout)$y
  x
}

reallocate_blocks <- function(x, rms) {
  provided <- !is.na(x$..fwa_provided_new_rm)
  is.na(x$rm[provided]) <- TRUE

  rle <- rle(x$rm)

  x$rm[provided] <- x$..fwa_provided_new_rm[provided]

  df <- data.frame(
    values = rle$values,
    length = rle$lengths
  )
  df$end <- cumsum(df$length)
  df$start <- df$end - df$length + 1


  df <- df[!is.na(df$values) & df$length > 1,]

  if(!nrow(df)) return(x)

  if(df$start[1] == 1) {
    df$start[1] <- 2
    df$length[1] <- df$length[1] - 1
  }

  nrm <- length(x$rm)
  ndf <- nrow(df)
  if(df$end[ndf] == nrm) {
    df$end[ndf] <- nrm - 1
    df$length[ndf] <- df$length[ndf] - 1
  }

  df <- df[df$length > 1,]

  if(!nrow(df)) return(x)

  xrev <- x
  for(i in nrow(df):1) {
    xrev <- interpolate_block(xrev, start = df$start[i], end = df$end[i])
  }
  for(i in 1:nrow(df)) {
    x <- interpolate_block(x, start = df$start[i], end = df$end[i])
    indices <- df$start[i]:df$end[i]
    x$rm[indices] <- pmean(x$rm[indices], xrev$rm[indices])
    for(j in indices) {
      x$rm[j] <- rms$rm[which.min(abs(x$rm[j] - rms$rm))]
    }
  }
  x
}

to_prev_max <- function(x) {
  prev_cummax <- prev_cummax(x$rm)
  wch <- which(x$rm < prev_cummax)
  x$rm[wch] <- prev_cummax[wch]
  x
}

update_rms <- function(x, rms) {
  provided <- !is.na(x$..fwa_provided_new_rm)
  x$rm[provided] <- x$..fwa_provided_new_rm[provided]

  wch <- which(provided)
  for(id in wch) {
    x$rm[1:id] <- pmin(x$rm[1:id], x$rm[id])
  }
  x <- to_prev_max(x)
  x <- reallocate_blocks(x, rms)
  x <- to_prev_max(x)
  if(!vld_sorted(x$rm)) {
    stop("generated new_rm should be sorted end")
  }
  x
}

snap_rm_to_rms <- function(x, rms) {
  chk_sorted(x$..fwa_provided_new_rm, x_name = "`x$new_rm`")

  rms <- rms |>
    dplyr::filter(.data$blk == x$new_blk[1])

  blk <- x$blk

  x <- x |>
    new_blk_to_blk() |>
    snap_rm_to_point(rms) |>
    update_rms(rms) |>
    distance_to_rm(rms) |>
    dplyr::mutate(new_blk = .data$blk)
  x$blk <- blk
  x
}

#' Snap River Meter to River Meters
#'
#' Assigns closest river meter to river meters based on blue line keys.
#' If x already includes new_rm column then non-missing values are preserved.
#' The non-missing new_rm values must be ordered (with respect to x$rm)
#' and must be present in rm$rm.
#' If x already includes new_blk then river meters can be assigned to
#' a creek with a different blue line key and/or
#' river meters from multiple creeks can be assigned to the same creek
#' (for example in the case of a previously unmapped side channel and the mainstem).
#'
#' The closest river meter is snapped to each rm (by blk = new_blk) and missing
#' new_rm values are replaced with the corresponding rm value.
#' The new_rm values are then ordered by adjusting the values so that
#' firstly all previous values are not greater than each provided new_rm value
#' and then all subsequent values are not less than the maximum previous value.
#' Next all runs of two or more identical new_rm values that do not include
#' a provided new_rm are interpolated between the previous and subsequent
#' new_rm values based on the original rm spacing and then snapped
#' to the closest rm value in rm.
#'
#' To ensure that pairs of streams snap at their mouths set the new_rm
#' to be 0 where the rm is 0 or set `snap_mouths = TRUE`
#'
#' @param x An sf object of spatial points with blk and rm columns and optional new_rm integer and new_blk columns.
#' @param rm An sf object of spatial point with blk and rm columns.
#' @param snap_mouths A flag specifying whether to snap pairs of streams at their mouths (rm = 0) where new_rm is not already set.
#' @return An updated version of x with integer columns blk, rm, new_blk, new_rm and numeric column distance_to_new_rm.
#' @export
#' @examples
#' rm <- fwa_add_rms_to_blk(data.frame(blk = 356308001))
#' x <- rm[rm$rm %in% c(0, 2000, 5000, 6000, 7000),]
#' rm <- rm[rm$rm %in% c(1000, 3000, 4000, 8000, 9000, 10000),]
#' fwa_snap_rm_to_rms(x, rm)
fwa_snap_rm_to_rms <- function(x, rm, snap_mouths = FALSE) {
  chk::chk_s3_class(x, "sf")
  chk::chk_s3_class(rm, "sf")
  chk_flag(snap_mouths)

  check_names(x, c("blk", "rm"))
  check_names(rm, c("blk", "rm"))
  chk_not_subset(colnames(x), c("..fwa_id", "..fwa_blk", "..fwa_provided_new_rm", "..fwa_x_rm", "..fwa_mouth"))

  chk_whole_numeric(x$blk)
  chk_not_any_na(x$blk)
  chk_gt(x$blk)

  chk_whole_numeric(x$rm)
  chk_not_any_na(x$rm)
  chk_gte(x$rm)
  check_key(x, c("blk", "rm"))

  chk_whole_numeric(rm$blk)
  chk_not_any_na(rm$blk)
  chk_gt(rm$blk)

  chk_whole_numeric(rm$rm)
  chk_not_any_na(rm$rm)
  chk_gte(rm$rm)
  check_key(rm, c("blk", "rm"))

  if(has_name(x, "new_rm")) {
    chk_whole_numeric(x$new_rm)
    chk_gte(x$new_rm)
    if(!vld_join(x[!is.na(x$new_rm),], rm, c(blk = "blk", new_rm = "rm"))) {
      chk::abort_chk("All `x$new_rm` values must be in `rm$rm` by `blk`")
    }
  }

  if(has_name(x, "new_blk")) {
    chk_whole_numeric(x$new_blk)
    chk_not_any_na(x$new_blk)
    chk_gt(x$new_blk)
  } else {
    x$new_blk <- x$blk
  }

  blks <- x |>
    dplyr::distinct(.data$blk, .data$new_blk)

  if(!vld_unique(blks$blk)) {
    abort_chk("Each blk in `x` must map to at most one new_blk.")
  }

  if(!nrow(x)) {
    x <- x |>
      tidyplus::add_missing_column(
        new_rm = integer(0),
        distance_to_new_rm = numeric(0)) |>
      relocate_blks_new_rm()

    return(x)
  }
  if(!nrow(rm)) {
    x <- x |>
      tidyplus::add_missing_column(
        new_rm = NA_integer_,
        distance_to_new_rm = NA_real_) |>
      relocate_blks_new_rm()
    return(x)
  }

  rm <- same_crs(rm, x)

  rm <- rm |>
    dplyr::select(rm, "blk", "rm")

  x <- x |>
    tidyplus::add_missing_column(new_rm = NA_integer_)

  x$new_rm <- as.integer(x$new_rm)

  if(snap_mouths) {
    mouths <- rm |>
      dplyr::as_tibble() |>
      dplyr::filter(.data$rm == 0) |>
      dplyr::select("blk", "rm") |>
      dplyr::mutate(..fwa_mouth = 0)

    x <- x |>
      dplyr::left_join(mouths, by = c(rm = "rm", new_blk = "blk")) |>
      tidyplus::coalesce_data(list(new_rm = c("new_rm", "..fwa_mouth")), quiet = TRUE)
  }

  x |>
    dplyr::arrange("blk", "rm") |>
    dplyr::mutate(..fwa_id = 1:dplyr::n()) |>
    dplyr::rename(..fwa_provided_new_rm = "new_rm",
                  ..fwa_x_rm = "rm") |>
    group_split_sf(.data$blk) |>
    lapply(snap_rm_to_rms, rm = rm) |>
    dplyr::bind_rows() |>
    dplyr::rename(new_rm = "rm",
                  distance_to_new_rm = "distance_to_rm",
                  rm = "..fwa_x_rm") |>
    dplyr::mutate(blk = as.integer(.data$blk),
                  new_blk = as.integer(.data$new_blk),
                  rm = as.integer(.data$rm),
                  new_rm = as.integer(.data$new_rm)) |>
    dplyr::select(!c("..fwa_id", "..fwa_blk", "..fwa_provided_new_rm")) |>
    relocate_blks_new_rm() |>
    dplyr::arrange(.data$blk, .data$rm)
}
