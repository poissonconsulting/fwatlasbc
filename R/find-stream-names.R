#' Find Stream Names
#'
#' Finds gnis stream names that match regular expression.
#'
#' @param pattern A string of a regular expression.
#' @param ignore_case A flag specifying whether to ignore case
#' when matching the regular expression to gnis stream names.
#' @return A tibble with character column StreamName of the names of
#' all the streams that match the regular expression.
#' @family streams
#' @export
#' @examples
#' fwa_find_stream_names("sangan")
fwa_find_stream_names <- function(pattern = ".*", ignore_case = TRUE){
  chk_string(pattern)
  chk_flag(ignore_case)

  stream_names <- unique(named_streams$StreamName)
  indices <- grep(pattern, stream_names, ignore.case = ignore_case)
  stream_names <- stream_names[indices]
  stream_names <- sort(stream_names)
  dplyr::tibble(StreamName = stream_names)
}
