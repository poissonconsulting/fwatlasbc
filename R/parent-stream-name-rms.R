#' Parent Stream Name
#'
#' Gets parent stream name.
#'
#' @param x A character vector of one or more stream names.
#' @param rms A data frame with integer columns blk and parent_blk.
#' There must be only one parent_blk for each blk.
#' @param stream_name A data frame with character column stream_name and integer column blk. There must be no more than one blk for the stream names specified.
#' @return A character vector of the parent blue line keys.
#' @export
fwa_parent_stream_name_rms <- function(x, rms, stream_name = fwatlasbc::fwa_stream_name) {
  chk_character_or_factor(x)

  check_names(rms, c("blk", "parent_blk"))

  chk_whole_numeric(rms$blk)
  chk_not_any_na(rms$blk)
  chk_gt(rms$blk)

  chk_whole_numeric(rms$parent_blk)
  chk_gt(rms$parent_blk)

  rms <- rms |>
    dplyr::distinct(.data$blk, .data$parent_blk)

  check_key(rms, c("blk", "parent_blk"))

  chk_data(stream_name)
  check_names(stream_name, c("blk", "stream_name"))
  chk_gt(stream_name$blk)
  chk_whole_numeric(stream_name$blk)
  chk_not_any_na(stream_name$blk)
  chk_character_or_factor(stream_name$stream_name)
  chk_not_any_na(stream_name$stream_name)

  sname <- stream_name |>
    dplyr::filter(.data$stream_name %in% x)

  if (anyDuplicated(sname$stream_name)) {
    abort_chk("Each `x` stream_name value must have one blk value in `stream_name`.")
  }

  if (!length(x)) return(character(0))
  if (!nrow(rms)) return(rep(NA_character_, length(x)))

  x <- x |>
    dplyr::tibble(stream_name = x) |>
    fwa_add_blks_to_stream_name(sname) |>
    dplyr::mutate(blk = fwa_parent_blk_rms(.data$blk, rms)) |>
    dplyr::select(!"stream_name") |>
    fwa_add_stream_names_to_blk(stream_name)

  x$stream_name
}
