add_rms_to_blk <- function(x, epsg) {
  check_dim(x, dim = nrow, values = 1L) # +chk

  interval <- x$..fwa_interval
  start <- x$..fwa_start
  end <- x$..fwa_end

  if(is.infinite(end)) end <- NULL

  rm <- fwa_locate_along_interval(x$BLK,
                                  interval_length = interval,
                                  start_measure = start,
                                  end_measure = end,
                                  epsg = epsg) |>
    dplyr::mutate(rm = .data$index * interval + start,
                  rm = as.integer(.data$rm),
                  Elevation = unname(sf::st_coordinates(.data$geometry)[,"Z"])) |>
    sf::st_zm(x) |>
    dplyr::select(.data$rm, .data$Elevation, .data$geometry)

  if(!is.null(end)) {
    lim <- floor((end - start) / interval)

    if(nrow(x) < lim)
      chk::wrn("`end` was not reached for BLK ", x$BLK)
  }
  x |>
    dplyr::bind_cols(rm) |>
    sf::st_set_geometry("geometry")
}

#' Add River Meters to Blue Line Key
#'
#' Adds distances (rm) and spatial coordinates (geometry) of
#' regularly spaced points along blue line key (BLK).
#' All distances which are in meters are from the river mouth.
#'
#' @param x A data frame with integer column BLK.
#' @param interval A whole numeric of the distance between points.
#' @param start A whole numeric of the start distance.
#' @param end An integer of the end distance.
#' @param epsg A positive whole number of EPSG projection for the coordinates.
#' @return An sf tibble with the columns of x plus integer column rm
#' and sf column geometry.
#' @family rm
#' @export
#' @examples
#' fwa_add_rms_to_blk(data.frame(BLK = 356308001))
fwa_add_rms_to_blk <- function(x, interval = 1000, start = 0, end = Inf,
                               epsg = getOption("fwa.epsg", 3005)){
  check_data(x)
  check_dim(x, dim = nrow, values = TRUE)
  chk_whole_numeric(x$BLK)
  chk_not_any_na(x$BLK)
  chk_gt(x$BLK)
  chk_unique(x$BLK)
  chk_not_subset(colnames(x), c("rm", "Elevation", "geometry"))
  chk_not_subset(colnames(x), c("..fwa_interval", "..fwa_start",
                                "..fwa_end", "..fwa_id"))

  chk_whole_number(interval)
  chk_gt(interval)
  chk_whole_number(start)
  chk_gte(start)
  chk_whole_number(end)
  chk_gt(end, start)
  chk_whole_number(epsg)
  chk_gte(epsg)

  x |>
    dplyr::as_tibble() |>
    dplyr::mutate(..fwa_interval = interval,
                  ..fwa_start = start,
                  ..fwa_end = end,
                  ..fwa_id = 1:dplyr::n()) |>
    dplyr::group_split(.data$BLK) |>
    lapply(add_rms_to_blk, epsg = epsg) |>
    dplyr::bind_rows() |>
    dplyr::arrange(.data$..fwa_id, .data$rm) |>
    dplyr::select(-.data$..fwa_interval,
                  -.data$..fwa_start,
                  -.data$..fwa_end,
                  -.data$..fwa_id)
}
