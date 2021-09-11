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
  wshed[c("blue_line_key", "distance_from_mouth", "area_ha", "geometry")]
}
