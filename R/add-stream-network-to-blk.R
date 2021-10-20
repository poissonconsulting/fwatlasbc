add_stream_network_to_blk <- function(x, epsg) {
  check_dim(x, dim = nrow, values = 1L) # +chk

  x <- x |>
    fwa_add_watershed_to_blk(rm = x$rm, epsg = epsg) |>
    fwa_add_collection_to_watershed(epsg = epsg) |>
    dplyr::filter(
      !is.na(.data$fwa_watershed_code) & stringr::str_detect(.data$fwa_watershed_code, "^999")
    )

  main <- x |>
    dplyr::filter(blue_line_key = x$blk) |>
    dplyr::slice_max(.data$upstream_route_measure)

  max_rm <- main$upstream_route_measure

  wshed_code <- main$fwa_watershed_code |>
    stringr::str_replace_all("-0{6,6}")

  x |>
    dplyr::filter(stringr::str_detect(.data$fwa_watershed_code == wshed_code)) |>
    dplyr::mutate(..fwa_percent = stringr::str_replace(.data$fwa_watershed_code, wshed_code),
                  ..fwa_percent = stringr::str_extract(.data$..fwa_percent, "^\\d{6,6}"),
                  ..fwa_percent = as.integer(.data$..fwa_percent),
                  ..fwa_percent = .data$..fwa_percent / 1000000) |>
    dplyr::filter(.data$..fwa_percent >= x$rm / max_rm)
}

#' Add Stream Network to Blue Line Key
#'
#' Adds stream network to blue line key upstream of rm based on watershed codes
#' and intersection of stream segment start points with blue line key.
#'
#' @inheritParams fwapgr::fwa_collection
#' @param x A data frame with a whole numeric blue line key (blk) column.
#' @param rm A whole numeric vector of the river meter.
#' @return An sf object
#' @seealso \code{\link[fwapgr]{fwa_collection}}.
#' @export
#' @examples
#' \dontrun{
#' fwa_add_stream_network_to_blk(data.frame(blk = 356308001))
#' }
fwa_add_stream_network_to_blk <- function(x, rm = 0,
                                          epsg = getOption("fwa.epsg", 3005)) {
  x |>
    dplyr::mutate(..fwa_id = 1:dplyr::n(),
                  ..fwa_rm = rm,
                  ..fwa_percent = NA_real_) |>
    dplyr::group_split(.data$..fwa_id) |>
    lapply(add_stream_network_to_blk, epsg = epsg) |>
    dplyr::bind_rows() |>
    dplyr::arrange(.data$..fwa_id) |>
    dplyr::select(-.data$..fwa_id, -.data$..fwa_rm, -.data$..fwa_percent)
}
