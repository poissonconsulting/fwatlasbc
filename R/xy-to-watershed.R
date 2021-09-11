
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
  wshed[c("area_ha", "geometry")]
}
