#' Search Streams
#'
#' Get stream names that match regular expression.
#'
#' @param stream_name A string of a regular expression.
#' By default returns all values.
#' @param ignore_case A flag indicating whether to ignore case when matching the regular expression.
#' @return A character vector of the stream names.
#' @export
#' @examples
#' fwa_search_streams("sangan")
fwa_search_streams <- function(stream_name = ".*", ignore_case = TRUE){
  chk_string(stream_name)
  chk_flag(ignore_case)
  x <- unique(as.character(named_streams$gnis_name))
  i <- grep(stream_name, x, ignore.case = ignore_case)
  sort(x[i])
}

#' Blue Line Key from Stream
#'
#' Gets blue line key from stream name.
#'
#' @param stream_name A string of the stream name.
#' @return A integer vector of blue line keys associated with the stream name.
#' If no associated blue line keys it returns an empty integer vector.
#' @export
#' @examples
#' fwa_stream_to_blue_line_keys("Sangan River")
fwa_stream_to_blue_line_keys <- function(stream_name){
  chk_string(stream_name)
  blue_line_key <- named_streams$gnis_name == stream_name
  blue_line_key <- named_streams$blue_line_key[blue_line_key]
  blue_line_key <- unique(blue_line_key)
  blue_line_key <- sort(blue_line_key)
  blue_line_key
}
