#' Add Blue Line Key(s) to Stream Name
#'
#' Adds blue line keys (blk) to stream names.
#' There may be more than one stream with the same name.
#'
#' @param x A data frame with character column stream_name.
#' @return A tibble with the columns of x plus an integer column blk.
#' @export
#' @examples
#' fwa_add_blks_to_stream_name(data.frame(stream_name = "Sangan River"))
fwa_add_blks_to_stream_name <- function(x) {
  check_data(x, values = list(stream_name = c("", NA)))
  chk_not_subset(colnames(x), "blk")

  x |>
    as_tibble() |>
    left_join(fwatlasbc::fwa_stream_name, by = "stream_name")
}
