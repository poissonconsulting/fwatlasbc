adjust_watershed <- function(wshed, x, epsg, nocache) {
  if (x$..fwa_exclude && wshed$refine_method == "DROP") {
    return(wshed)
  }
  if (!x$..fwa_exclude && wshed$refine_method == "KEEP") {
    return(wshed)
  }

  fwshed <- fwa_watershed_hex(
    blue_line_key = x$blk,
    downstream_route_measure = x$rm,
    epsg = epsg,
    nocache = nocache
  )

  fgeometry <- sf::st_union(fwshed$geometry)

  if (x$..fwa_exclude) {
    wshed$geometry <- sf::st_difference(wshed$geometry, fgeometry)
  } else {
    wshed$geometry <- sf::st_union(wshed$geometry, fgeometry)
  }
  wshed
}

add_watershed_to_blk <- function(x, epsg, nocache) {
  check_dim(x, dim = nrow, values = 1L) # +chk

  wshed <- fwa_watershed_at_measure(
    blue_line_key = x$blk,
    downstream_route_measure = x$rm,
    epsg = epsg,
    nocache = nocache
  )

  if (!nrow(wshed)) {
    return(
      x |>
        dplyr::mutate(
          ..fwa_empty = TRUE,
          geometry = sf::st_sfc(sf::st_polygon(), crs = epsg)
        ) |>
        sf::st_set_geometry("geometry")
    )
  }

  wshed <- adjust_watershed(wshed, x, epsg, nocache = nocache)

  x |>
    dplyr::mutate(..fwa_empty = FALSE, geometry = wshed$geometry) |>
    sf::st_set_geometry("geometry")
}

#' Add Watershed to Blue Line Key
#'
#' Adds polygon (geometry) of aggregated fundamental watersheds to blue line key (blk).
#' The rm distances which is in meters is from the river mouth.
#' If no watershed is returned, which can occur if the rm is in the headwaters
#' of the stream or beyond its upstream end, the geometry is an empty polygon
#' and a warning is given with the number of empty watersheds.
#' @return An sf tibble with the columns of x plus
#' sf column geometry.
#'
#' @inheritParams fwapgr::fwa_locate_along
#' @param x An sf object with a polygon sfc column specifying watersheds
#' and an optional rm column specfying the river meter.
#' The rm is set to be 0 if missing.
#' @param exclude A logical vector specifying whether to exclude the
#' fundamental watershed in which the start falls.
#' @return A sf object
#' @seealso [fwapgr::fwa_watershed_at_measure()].
#' @export
#' @examples
#' \dontrun{
#' fwa_add_watershed_to_blk(data.frame(blk = 356308001))
#' }
fwa_add_watershed_to_blk <- function(
  x,
  exclude = FALSE,
  epsg = getOption("fwa.epsg", 3005),
  nocache = getOption("fwa.nocache", FALSE)
) {
  check_data(x)
  check_dim(x, dim = nrow, values = TRUE)
  chk_whole_numeric(x$blk)
  chk_not_any_na(x$blk)
  chk_gt(x$blk)
  if (!"rm" %in% names(x)) {
    x$rm <- NA_real_
  }
  chk_numeric(x$rm)
  chk_gte(x$rm)
  x$rm[is.na(x$rm)] <- 0

  chk_not_subset(colnames(x), "geometry")
  chk_not_subset(colnames(x), c("..fwa_exclude", "..fwa_id", "..fwa_empty"))
  chk_logical(exclude)
  chk_not_any_na(exclude)
  chk_whole_number(epsg)
  chk_gte(epsg)

  x <- x |>
    dplyr::as_tibble() |>
    dplyr::mutate(
      ..fwa_exclude = exclude,
      ..fwa_id = seq_len(dplyr::n())
    ) |>
    group_split_sf(.data$blk) |>
    lapply(add_watershed_to_blk, epsg = epsg, nocache = nocache) |>
    dplyr::bind_rows() |>
    dplyr::arrange(.data$..fwa_id)

  n_empty <- sum(x$..fwa_empty)
  if (n_empty) {
    wrn(
      "%n watershed%s %r empty as no watershed was returned ",
      "(the rm may be in the headwaters or beyond the end of the stream).",
      n = n_empty
    )
  }

  x |>
    dplyr::select(!c("..fwa_exclude", "..fwa_id", "..fwa_empty"))
}
