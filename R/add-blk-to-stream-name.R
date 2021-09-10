#' Adds Blue Line Key to Stream Name
#'
#' Adds blue line keys to stream names.
#'
#' @param x A data frame with column stream_name.
#' @return An copy of x with additional blue_line_key column.
#' @export
#' @family streams
#' @examples
#' fwa_add_blk_to_stream_name(data.frame(stream_name = "Sangan River"))
fwa_add_blk_to_stream_name <- function(x) {
  check_data(x, values = list(stream_name = c("", NA)))

  x$blk<- NULL

  if(!nrow(x)) {
    x$blk <- integer(0)
    return(x)
  }
  if(anyNA(x$stream_name)) {
    x$blk <- NA_integer_
    return(x)
  }
  x |>
    left_join(named_streams, by = c(stream_name = "gnis_name")) |>
    rename(blk = .data$blue_line_key)
}
