#' Adds Blue Line Key to Stream Name
#'
#' Adds blue line keys (BLK) to stream names.
#' A stream name may correspond to more than one blue line key (BLK).
#'
#' @param x A data frame with character column StreamName.
#' @return A tibble with the columns of x plus an integer column BLK.
#' @export
#' @family streams
#' @examples
#' fwa_add_blk_to_stream_name(data.frame(StreamName = "Sangan River"))
fwa_add_blk_to_stream_name <- function(x) {
  check_data(x, values = list(StreamName = c("", NA)))

  x |>
    as_tibble() |>
    left_join(named_streams, by = c(StreamName = "gnis_name")) |>
    rename(BLK = .data$blue_line_key)
}
