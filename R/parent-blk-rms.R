#' Parent Blue Line Key
#'
#' Gets parent blue line key.
#'
#' @param x A whole numeric vector of one or more blue line keys.
#' @param rms A data frame with integer columns blk and parent_blk.
#' There must be only one parent_blk for each blk.
#' @return A whole numeric vector of the parent blue line keys.
#' @export
fwa_parent_blk_rms <- function(x, rms) {
  chk_whole_numeric(x)
  chk_gte(x)

  check_names(rms, c("blk", "parent_blk"))

  chk_whole_numeric(rms$blk)
  chk_not_any_na(rms$blk)
  chk_gt(rms$blk)

  chk_whole_numeric(rms$parent_blk)
  chk_gt(rms$parent_blk)

  rms <- rms |>
    dplyr::distinct(.data$blk, .data$parent_blk)

  if (anyDuplicated(rms$blk)) {
    abort_chk("Each blk in `rms` must have one parent_blk value.")
  }

  if (!length(x)) return(integer(0))
  if (!nrow(rms)) return(rep(NA_integer_, length(x)))

  x <- dplyr::tibble(blk = x) |>
    dplyr::left_join(rms, by = "blk")

  as.integer(x$parent_blk)
}
