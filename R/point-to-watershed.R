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
