add_rm_to_lon_lat <- function(x, limit, tolerance, epsg) {
  check_dim(x, dim = nrow, values = 1L) # +chk

  rm <- fwapgr::fwa_index_point(x = x$lon, y = x$lat,
                                limit = limit, tolerance = tolerance,
                                  epsg = epsg) |>
    dplyr::select(blk = .data$blue_line_key,
                  rm = .data$downstream_route_measure,
                  distance_to_lon_lat = .data$distance_to_stream,
                  .data$geometry)

  x |>
    dplyr::bind_cols(rm) |>
    sf::st_set_geometry("geometry")
}

#' Add River Meters to Longitude and Latitude
#'
#' Adds numeric river meter (rm) and integer blue line key (blk) column
#' and sfc point (geometry) column
#' of the closest point on the stream network (within 5 km)
#' to the point specified by the lon and lat (WGS84).
#'
#' @param x A data frame with numeric longitude (long) and
#' latitude (lat) columns.
#' @inheritParams fwapgr::fwa_index_point
#' @param epsg A positive whole number of the EPSG projection for the geometry.
#' @return An sf tibble with the columns of x plus integer column blk,
#' real columns rm and distance_to_lon_lat and sfc point column geometry
#' @family rm
#' @export
#' @examples
#' fwa_add_rm_to_lon_lat(data.frame(lon = -132.26, lat = 53.36))
fwa_add_rm_to_lon_lat <- function(x, tolerance = 5000, limit = 1,
                                  epsg = getOption("fwa.epsg", 3005)) {
  check_data(x, values = list(lon = c(-180,180,NA), lat = c(-90,90,NA)))
  check_dim(x, dim = nrow, values = TRUE)
  chk_not_subset(colnames(x), c("blk", "rm", "distance_to_lon_lat", "geometry"))
  chk_not_subset(colnames(x), "..fwa_id")
  chk_whole_number(epsg)
  chk_gte(epsg)

  x |>
    dplyr::as_tibble() |>
    dplyr::mutate(..fwa_id = 1:dplyr::n()) |>
    dplyr::group_split(.data$..fwa_id) |>
    lapply(add_rm_to_lon_lat, tolerance = tolerance, limit = limit, epsg = epsg) |>
    dplyr::bind_rows() |>
    dplyr::arrange(.data$..fwa_id) |>
    dplyr::select(-.data$..fwa_id)
}
