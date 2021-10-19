#' Add Blue Line Key(s) to Stream Name
#'
#' Adds blue line keys (blk) to stream names.
#' There may be more than one stream with the same name.
#'
#' @param x A data frame with character column StreamName.
#' @return A tibble with the columns of x plus an integer column blk.
#' @export
#' @family streams
#' @examples
#' fwa_add_blks_to_stream_name(data.frame(StreamName = "Sangan River"))
fwa_add_blks_to_stream_name <- function(x) {
  check_data(x, values = list(StreamName = c("", NA)))
  chk_not_subset(colnames(x), "blk")

  x |>
    as_tibble() |>
    left_join(named_streams, by = "StreamName")
}
