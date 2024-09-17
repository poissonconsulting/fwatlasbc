#' Converts Stream Names to Blue Line Keys
#'
#' Each stream name is converted to a blue line key
#' by calculating it's integer hash.
#'
#' This function is only expected to be used when a blue line
#' key does not already exist.
#'
#' @param names A character vector of stream names.
#'
#' @return An positive integer vector of blue line keys.
#' @export
#'
#' @examples
#' fwa_convert_stream_names_to_blks(c("a stream name", "a stream name2"))
fwa_convert_stream_names_to_blks <- function(names) {
  chk_null_or(names, vld = vld_character_or_factor)
  if (!length(names) || is.null(names)) return(integer())
  names_chr <- as.character(names)
  names_na <- is.na(names_chr)
  names_int <- abs(digest::digest2int(names_chr))
  is.na(names_int) <- names_na
  names_int
}
