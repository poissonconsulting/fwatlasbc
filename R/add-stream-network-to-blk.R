add_stream_network_to_blk <- function(x, epsg) {
  check_dim(x, dim = nrow, values = 1L) # +chk

  x <- x |>
    fwa_add_watershed_to_blk(epsg = epsg) |>
    fwa_add_collection_to_watershed(epsg = epsg) |>
    dplyr::filter(
      !is.na(.data$fwa_watershed_code) & !stringr::str_detect(.data$fwa_watershed_code, "^999")
    )

  main <- x |>
    dplyr::filter(.data$blue_line_key == x$blk)  |>
    dplyr::slice_max(.data$upstream_route_measure)

  max_rm <- main$upstream_route_measure

  wshed_code <- main$fwa_watershed_code |>
    stringr::str_replace_all("-0{6,6}", "")

  print(wshed_code)

  x |>
    dplyr::filter(stringr::str_detect(.data$fwa_watershed_code, wshed_code)) |>
    dplyr::mutate(
      ..fwa_percent2 = stringr::str_replace(.data$fwa_watershed_code, paste0("^", wshed_code, "-"), ""),
      ..fwa_percent2 = stringr::str_extract(.data$..fwa_percent2, "^\\d{6,6}"),
      ..fwa_percent2 = as.integer(.data$..fwa_percent2),
      ..fwa_percent2 = .data$..fwa_percent2 / 1000000
    ) |>
    dplyr::filter(.data$..fwa_percent2 >= .data$rm / max_rm)
}

#' Add Stream Network to Blue Line Key
#'
#' Adds stream network to blue line key upstream of rm based on watershed codes
#' and intersection of stream segment start points with blue line key.
#'
#' @inheritParams fwapgr::fwa_collection
#' @param x A data frame with a whole numeric blue line key (blk) column
#' and an optional rm column specfying the river meter.
#' The rm is set to be 0 if missing.
#'
#' @return An sf object
#' @seealso \code{\link[fwapgr]{fwa_collection}}.
#' @export
#' @examples
#' \dontrun{
#' fwa_add_stream_network_to_blk(data.frame(blk = 356308001))
#' }
fwa_add_stream_network_to_blk <- function(x,
                                          epsg = getOption("fwa.epsg", 3005)) {

  check_data(x)
  check_dim(x, dim = nrow, values = TRUE)
  chk_whole_numeric(x$blk)
  chk_not_any_na(x$blk)
  chk_gt(x$blk)

  if(!"rm" %in% names(x)) x$rm <- NA_real_
  chk_numeric(x$rm)
  chk_gte(x$rm)
  x$rm[is.na(x$rm)] <- 0

  x |>
    dplyr::mutate(..fwa_id2 = 1:dplyr::n(),
                  ..fwa_percent2 = NA_real_) |>
    dplyr::group_split(.data$..fwa_id2) |>
    lapply(add_stream_network_to_blk, epsg = epsg) |>
    dplyr::bind_rows() |>
    dplyr::arrange(.data$..fwa_id2) |>
    dplyr::select(-.data$..fwa_id2, -.data$..fwa_percent2)
}
