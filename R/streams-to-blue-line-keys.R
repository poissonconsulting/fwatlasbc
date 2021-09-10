#' Blue Line Keys from Stream Names
#'
#' Gets blue line keys from stream names.
#'
#' @param stream_name A character vector of stream names.
#' @return A tibble of the stream name and blue line key.
#' @export
#' @family streams
#' @examples
#' fwa_streams_to_blue_line_keys("Sangan River")
fwa_streams_to_blue_line_keys <- function(stream_name) {
  lifecycle::deprecate_soft("0.0.1", "fwa_streams_to_blue_line_keys()",
                            "fwa_add_blk_to_stream_name()")

  chk_character(stream_name)
  named_streams |>
    dplyr::filter(.data$gnis_name %in% stream_name) |>
    dplyr::select(stream_name = .data$gnis_name,
                  .data$blue_line_key)
}
