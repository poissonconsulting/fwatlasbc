#' FWA rkm
#'
#' Provided a blue_line_key and starting distance upstream from river mouth, return Rkm points along an interval.
#'
#' @param blue_line_key An integer of the stream blue line key.
#' @param interval An integer of the interval distance in meters.
#' @param start An integer of the distance in meters upstream from the river mouth to start from.
#' @param epsg A positive whole number of the epsg to transform features to.
#' @param limit A positive whole number of the maximum number of features to return.
#' @return A sf object
#' @export
#' @examples
#' fwa_rkm(356308001)
fwa_rkm <- function(blue_line_key, interval = 1000, start = 0, epsg = 4326, limit = 500){
  chk_whole_number(blue_line_key)
  chk_whole_number(interval)
  chk_whole_number(start)
  chk_whole_number(epsg)

  x <- fwa_locate_along_interval(blue_line_key, interval_length = interval, start = start, epsg = epsg, limit = limit)
  x$rkm <- x$index*interval/1000
  x$blue_line_key <- blue_line_key
  x[c("blue_line_key", "rkm", "geometry")]
}
