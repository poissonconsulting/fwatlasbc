#' FWA point to watershed
#'
#' Get watershed polygon from a sf or sfc point starting from nearest point along a FWA stream.
#'
#' @param point A single row sf object or sfc object of the point (sfc geometry must inherit from sfc_POINT).
#' @return A sf object
#' @seealso \code{\link[fwapgr]{fwa_watershed_at_measure}} and \code{\link[fwapgr]{fwa_index_point}}.
#' @export
#' @examples
#' \dontrun{
#' sf_point <- sf::st_as_sf(sf::st_as_sfc("POINT(-131.927 54.032)", crs = 4326))
#' fwa_point_to_watershed(sf_point)
#' }
fwa_point_to_watershed <- function(point) {
  chk_sf_sfc(point)
  point <- sf::st_geometry(point)
  chk_sfc_point(point)
  coords <- sf::st_coordinates(point)
  epsg <- get_epsg(point)
  fwa_xy_to_watershed(x = coords[1], y = coords[2], epsg = epsg)
}

#' FWA xy to watershed
#'
#' Get watershed polygon from x coordinate, y coordinate and epsg starting from nearest point along a FWA stream.
#'
#' @inheritParams fwapgr::fwa_collection
#' @param x A number of the x coordinate.
#' @param y A number of the y coordinate.
#' @return A sf object
#' @seealso \code{\link[fwapgr]{fwa_watershed_at_measure}} and \code{\link[fwapgr]{fwa_index_point}}.
#' @export
#' @examples
#' \dontrun{
#' sf_point <- sf::st_as_sf(sf::st_as_sfc("POINT(-131.927 54.032)", crs = 4326))
#' fwa_xy_to_watershed(x = -131.927, y = 54.032, epsg = 4326)
#' }
fwa_xy_to_watershed <- function(x, y, epsg = getOption("fwa.epsg", 3005)) {
  chk_number(x)
  chk_number(y)
  chk_whole_number(epsg)

  index <- fwapgr::fwa_index_point(x = x, y = y, srid = epsg)
  wshed <- fwa_watershed_at_measure(blue_line_key = index$blue_line_key,
                           downstream_route_measure = index$downstream_route_measure,
                           epsg = epsg)
  wshed["geometry"]
}

#' FWA blue_line_key to watershed
#'
#' Get watershed polygon from blue_line_kay starting from a distance upstream of the river mouth.
#'
#' @inheritParams fwapgr::fwa_locate_along
#' @param distance_from_mouth A positive whole number of the distance in meters upstream from the river mouth.
#' @return A sf object
#' @seealso \code{\link[fwapgr]{fwa_watershed_at_measure}}.
#' @export
#' @examples
#' \dontrun{
#' fwa_blue_line_key_to_watershed(356308001)
#' }
fwa_blue_line_key_to_watershed <- function(blue_line_key,
                                           distance_from_mouth = 0,
                                           epsg = getOption("fwa.epsg", 3005)) {
  chk_whole_number(blue_line_key)
  chk_gt(blue_line_key)
  chk_whole_number(distance_from_mouth)
  chk_gte(distance_from_mouth)

  wshed <- try(fwa_watershed_at_measure(blue_line_key = blue_line_key,
                            downstream_route_measure = distance_from_mouth,
                            epsg = epsg), silent = TRUE)

  if(is_try_error(wshed) && distance_from_mouth == 0)
    abort_chk("Unable to retrieve requested watershed. Try a non-zero `distance_from_mouth` value.")

  wshed$blue_line_key <- blue_line_key
  wshed$distance_from_mouth <- distance_from_mouth
  wshed[c("blue_line_key", "distance_from_mouth", "geometry")]
}
