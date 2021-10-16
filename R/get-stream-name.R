#' Get Stream Name
#'
#' Get gnis stream names that match regular expression.
#'
#' @param stream_name A string of a regular expression.
#' @param ignore_case A flag specifying whether to ignore case
#' when matching the regular expression.
#' @return A tibble with column StreamName of the names of
#' all the streams that match the regular expression.
#' @family streams
#' @export
#' @examples
#' fwa_get_stream_name("sangan")
fwa_get_stream_name <- function(pattern = ".*", ignore_case = TRUE){
  chk_string(pattern)
  chk_flag(ignore_case)

  stream_names <- unique(named_streams$gnis_name)
  indices <- grep(pattern, stream_names, ignore.case = ignore_case)
  stream_names <- stream_names[indices]
  stream_names <- sort(stream_names)
  tibble::tibble(StreamName = stream_names)
}
