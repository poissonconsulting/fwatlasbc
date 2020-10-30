#' FWA List Named streams
#'
#' List information on named streams including gnis_name.
#'
#' @param gnis_name A regular expression to filter table by gnis_name.
#' By default returns all values.
#' @param ignore_case A flag indicating whether to ignore case in regular expression.
#' @return A tibble of the named streams, including stream order and watershed group code.
#' @export
#' @examples
#' fwa_list_gnis_streams()
fwa_list_gnis_streams <- function(gnis_name = ".*", ignore_case = TRUE){
  chk_string(gnis_name)
  chk_flag(ignore_case)
  i <- grep(gnis_name, fwatlasbc::named_streams$gnis_name, ignore.case = ignore_case)
  fwatlasbc::named_streams[i,]
}

#' FWA Search Named streams
#'
#' Get stream gnis_names that match with regular expression.
#'
#' @param gnis_name A regular expression to match gnis_name.
#' By default returns all values.
#' @param ignore_case A flag indicating whether to ignore case in regular expression.
#' @return A character vector of the stream names.
#' @export
#' @examples
#' fwa_search_gnis_streams("sangan")
fwa_search_gnis_streams <- function(gnis_name = ".*", ignore_case = TRUE){
  chk_string(gnis_name)
  chk_flag(ignore_case)
  x <- unique(as.character(fwatlasbc::named_streams$gnis_name))
  i <- grep(gnis_name, x, ignore.case = ignore_case)
  x[i]
}
