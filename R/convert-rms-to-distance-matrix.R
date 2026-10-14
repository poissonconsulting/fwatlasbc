stream_path <- function(blk, streams) {
  path <- blk
  rm <- numeric()
  index <- match(blk, streams$blk)
  while (!is.na(index) && !is.na(streams$parent_blk[index])) {
    parent_blk <- streams$parent_blk[index]
    if (parent_blk %in% path) {
      abort_chk("`streams` must not be circular (blk ", blk, ").")
    }
    path <- c(path, parent_blk)
    rm <- c(rm, streams$parent_rm[index])
    index <- match(parent_blk, streams$blk)
  }
  list(blk = path, rm = rm)
}

# For stream i relative to stream j: whether i is on their lowest common
# stream, the distance from the mouth of i to the lowest common stream and the
# river meter at which the path from i reaches the lowest common stream.
stream_junctions <- function(paths) {
  n <- length(paths)
  on <- matrix(FALSE, n, n)
  distance <- matrix(Inf, n, n)
  rm <- matrix(NA_real_, n, n)
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      k <- match(TRUE, paths[[i]]$blk %in% paths[[j]]$blk)
      if (is.na(k)) {
        next
      }
      on[i, j] <- k == 1L
      distance[i, j] <- sum(paths[[i]]$rm[seq_len(max(k - 2L, 0L))])
      if (k > 1L) {
        rm[i, j] <- paths[[i]]$rm[k - 1L]
      }
    }
  }
  list(on = on, distance = distance, rm = rm)
}

#' Convert River Meters to Distance Matrix
#'
#' Calculates the distances along the stream network between river meters.
#' The distance is `Inf` if the river meters are on streams that are not
#' connected.
#'
#' The stream network is defined by the parent blue line key (`parent_blk`)
#' and the river meter on the parent where each stream joins it (`parent_rm`)
#' as returned by [`fwa_add_parent_to_streams()`].
#' Parents that are not in `streams$blk` are treated as root streams.
#'
#' @param x A data frame (including sf objects) with whole numeric column blk
#' and numeric column rm of the river meters.
#' All blk values must be in `streams$blk` and all rm values must be
#' non-negative.
#' @param streams A data frame with whole numeric column blk, whole numeric
#' column parent_blk and numeric column parent_rm.
#' @param direction A flag specifying whether to return negative distances
#' when the river meter in the row is upstream of the river meter in the
#' column.
#' @return A numeric matrix of the distances in m with one row and one column
#' for each row in `x`.
#' @export
#' @seealso [`fwa_add_parent_to_streams()`]
#' @examples
#' streams <- data.frame(
#'   blk = c(1, 2, 3),
#'   parent_blk = c(NA, 1, NA),
#'   parent_rm = c(NA, 400, NA)
#' )
#' rms <- data.frame(blk = c(1, 1, 2, 3), rm = c(100, 500, 50, 10))
#' fwa_convert_rms_to_distance_matrix(rms, streams)
#' fwa_convert_rms_to_distance_matrix(rms, streams, direction = TRUE)
fwa_convert_rms_to_distance_matrix <- function(x, streams, direction = FALSE) {
  chk_data(x)
  check_names(x, c("blk", "rm"))
  chk_data(streams)
  check_names(streams, c("blk", "parent_blk", "parent_rm"))
  chk_flag(direction)

  chk_whole_numeric(x$blk)
  chk_not_any_na(x$blk)
  chk_gt(x$blk)
  chk_subset(x$blk, streams$blk, x_name = "`x$blk`")

  chk_numeric(x$rm)
  chk_not_any_na(x$rm)
  chk_gte(x$rm, x_name = "`x$rm`")

  chk_whole_numeric(streams$blk)
  chk_not_any_na(streams$blk)
  chk_gt(streams$blk)

  chk_whole_numeric(streams$parent_blk)
  chk_gt(streams$parent_blk)

  chk_numeric(streams$parent_rm)
  chk_gte(streams$parent_rm, x_name = "`streams$parent_rm`")

  streams <- streams |>
    dplyr::as_tibble() |>
    dplyr::distinct(.data$blk, .data$parent_blk, .data$parent_rm)

  if (anyDuplicated(streams$blk)) {
    abort_chk(
      "Each blk in `streams` must have one parent_blk and parent_rm value."
    )
  }
  if (!identical(is.na(streams$parent_blk), is.na(streams$parent_rm))) {
    abort_chk(
      "`streams$parent_rm` must be missing if and only if ",
      "`streams$parent_blk` is missing."
    )
  }

  blk <- unique(x$blk)
  paths <- purrr::map(blk, stream_path, streams = streams)
  junctions <- stream_junctions(paths)

  n <- nrow(x)
  index_a <- rep(match(x$blk, blk), times = n)
  index_b <- rep(match(x$blk, blk), each = n)
  rm_a <- rep(x$rm, times = n)
  rm_b <- rep(x$rm, each = n)
  ab <- cbind(index_a, index_b)
  ba <- cbind(index_b, index_a)

  on_a <- junctions$on[ab]
  on_b <- junctions$on[ba]
  position_a <- ifelse(on_a, rm_a, junctions$rm[ab])
  position_b <- ifelse(on_b, rm_b, junctions$rm[ba])
  distance_a <- ifelse(on_a, 0, rm_a + junctions$distance[ab])
  distance_b <- ifelse(on_b, 0, rm_b + junctions$distance[ba])

  distance <- distance_a + distance_b + abs(position_a - position_b)
  distance[is.infinite(junctions$distance[ab])] <- Inf

  if (direction) {
    upstream <- on_b & position_a >= position_b
    upstream <- upstream & !is.na(upstream)
    distance[upstream] <- -distance[upstream]
  }
  matrix(distance, nrow = n, ncol = n)
}
