#' Search Streams
#'
#' Get stream names that match regular expression.
#'
#' @param stream_name A string of a regular expression that
#' by default returns all values.
#' @param ignore_case A flag indicating whether to ignore case
#' when matching the regular expression.
#' @return A character vector of the stream names.
#' @family streams
#' @export
#' @examples
#' fwa_search_streams("sangan")
fwa_search_streams <- function(stream_name = ".*", ignore_case = TRUE){
  chk_string(stream_name)
  chk_flag(ignore_case)
  stream_names <- unique(named_streams$gnis_name)
  indices <- grep(stream_name, stream_names, ignore.case = ignore_case)
  sort(stream_names[indices])
}
