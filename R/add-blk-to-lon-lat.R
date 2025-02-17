add_blk_to_lon_lat <- function(x, limit, tolerance, epsg, nocache) {
  check_dim(x, dim = nrow, values = 1L) # +chk

  rm <- fwapgr::fwa_index_point(
    x = x$lon, y = x$lat,
    limit = limit, tolerance = tolerance,
    epsg = epsg, nocache = nocache
  )
  if (!nrow(rm)) {
    rm <- rm |>
      dplyr::mutate(
        blk = integer(0),
        rm = numeric(0),
        distance_to_lon_lat = numeric(0)
      ) |>
      dplyr::select(
        "blk", "rm", "distance_to_lon_lat",
        "geometry"
      )

    x <- x |>
      dplyr::bind_cols(rm) |>
      sf::st_set_geometry("geometry")

    return(x)
  }

  rm <- rm |>
    dplyr::select(
      blk = "blue_line_key",
      rm = "downstream_route_measure",
      distance_to_lon_lat = "distance_to_stream",
      "geometry"
    )

  x |>
    dplyr::bind_cols(rm) |>
    sf::st_set_geometry("geometry")
}

#' Add Blue Line Key to Longitude and Latitude
#'
#' Adds integer blue line key (blk) and numeric river meter (rm) column
#' and sfc point (geometry) column
#' of the closest point on the stream network (by default within 5 km)
#' to the point specified by the lon and lat (WGS84).
#'
#' If a match isn't found the row is dropped.
#'
#' @param x A data frame with numeric longitude (long) and
#' latitude (lat) columns.
#' @inheritParams fwapgr::fwa_index_point
#' @param epsg A positive whole number of the EPSG projection for the geometry.
#' @return An sf tibble with the columns of x plus integer column blk,
#' real columns rm and distance_to_lon_lat and sfc point column geometry
#' @export
#' @examples
#' \dontrun{
#' fwa_add_blk_to_lon_lat(data.frame(lon = -132.26, lat = 53.36))
#' }
fwa_add_blk_to_lon_lat <- function(x, tolerance = 5000, limit = 1,
                                   epsg = getOption("fwa.epsg", 3005), nocache = getOption("fwa.nocache", FALSE)) {
  check_data(x, values = list(lon = c(-180, 180, NA), lat = c(-90, 90, NA)))
  check_dim(x, dim = nrow, values = TRUE)
  chk_not_subset(colnames(x), c("blk", "rm", "distance_to_lon_lat", "geometry"))
  chk_not_subset(colnames(x), "..fwa_id")
  chk_whole_number(epsg)
  chk_gte(epsg)

  x |>
    dplyr::as_tibble() |>
    dplyr::mutate(..fwa_id = seq_len(dplyr::n())) |>
    group_split_sf(.data$..fwa_id) |>
    lapply(add_blk_to_lon_lat, tolerance = tolerance, limit = limit, epsg = epsg, nocache = nocache) |>
    dplyr::bind_rows() |>
    dplyr::arrange(.data$..fwa_id) |>
    dplyr::select(!"..fwa_id")
}
