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
#' @param stream_name A vector of character strings of the stream names.
#' @return A data.frame including blue_line_key and watershed_group_code
#' values associated with each stream name.
#' If no associated blue line keys it returns an empty integer vector.
#' @export
#' @examples
#' fwa_streams_to_blue_line_keys("Sangan River")
fwa_streams_to_blue_line_keys <- function(stream_name){
  chk_character(stream_name)
  index <- named_streams$gnis_name %in% stream_name
  df <- named_streams[index,]
  df <- df[c("gnis_name", "blue_line_key", "watershed_group_code")]
  names(df)[names(df) == "gnis_name"] <- "stream_name"
  df
}
