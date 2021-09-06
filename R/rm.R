#' Get River Metre
#'
#' Provided a blue_line_key and start and possibly end distance upstream from
#' river mouth, return river metre points at regular intervals.
#'
#' @param blue_line_key An integer of the stream blue line key.
#' @param interval An integer of the interval distance in meters.
#' @param start An integer of the distance in meters upstream from the river mouth to start from.
#' @param end An integer of the distance in meters upstream from the river mouth to start from.
#' @param epsg A positive whole number of the epsg to transform features to.
#' @param limit A positive whole number of the maximum number of features to return.
#' @return A sf object
#' @family rm
#' @export
#' @examples
#' fwa_rm(356308001)
fwa_rm <- function(blue_line_key, interval = 1000, start = 0,
                   end = NULL,
                   epsg = getOption("fwa.epsg", 3005),
                   limit = getOption("fwa.limit", 10000)){

  chk_whole_number(blue_line_key)
  chk_gt(blue_line_key)
  chk_whole_number(interval)
  chk_whole_number(start)
  chk_null_or(end, vld = vld_whole_number)

  chk_gt(interval)
  chk_gte(start)
  chk_null_or(end, vld = vld_gt, value = start)

  if(!is.null(limit) && !is.null(end)) {
    lim <- floor((end - start) / interval)
    if(lim > limit)
      chk::abort_chk("`limit` must be greater than `(end - start) / interval` (", lim ,") not ", limit, ".")
  }

  x <- fwa_locate_along_interval(blue_line_key,
                                 interval_length = interval,
                                 start_measure = start,
                                 end_measure = end,
                                 epsg = epsg,
                                 limit = limit)
  x$rm <- x$index * interval + start

  if(!is.null(limit)) {
    if(nrow(x) == limit)
      chk::wrn("`limit` was reached.")
  }
  if(!is.null(end)) {
    lim <- floor((end - start) / interval)

    if(nrow(x) < lim)
      chk::wrn("`end` was not reached.")
  }

  x$blue_line_key <- as.integer(blue_line_key)
  x$rm <- as.integer(x$rm)
  x[c("blue_line_key", "rm", "geometry")]
}
