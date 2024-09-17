#' Add Blue Line Key(s) to Stream Name
#'
#' Adds blue line keys (blk) to stream names.
#' There may be more than one stream with the same name.
#'
#' @param x A data frame with character column stream_name.
#' @param stream_name A data frame with whole numeric column blk and character column stream_name.
#' @return A tibble with the columns of x plus an integer column blk.
#' @export
#' @examples
#' fwa_add_blks_to_stream_name(data.frame(stream_name = "Sangan River"))
fwa_add_blks_to_stream_name <- function(x, stream_name = fwatlasbc::fwa_stream_name) {
  check_data(x, values = list(stream_name = c("", NA)))
  chk_not_subset(colnames(x), "blk")

  chk_data(stream_name)
  check_names(stream_name, c("blk", "stream_name"))
  chk_gt(stream_name$blk)
  chk_whole_numeric(stream_name$blk)
  chk_character_or_factor(stream_name$stream_name)

  stream_name <- stream_name |>
    as_tibble() |>
    dplyr::select("blk", "stream_name") |>
    dplyr::distinct()

  if (!"sf" %in% class(x) && !"tbl" %in% class(x)) {
    x <- x |> as_tibble()
  }

  x |>
    left_join(stream_name, by = "stream_name", multiple = "all")
}
