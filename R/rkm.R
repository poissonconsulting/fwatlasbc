#' Get Rkm
#'
#' Provided a blue_line_key and starting distance upstream from river mouth, return river kilometre  points along an interval.
#'
#' @param blue_line_key An integer of the stream blue line key.
#' @param interval An integer of the interval distance in meters.
#' @param distance_upstream An integer of the distance in meters upstream from the river mouth to start from.
#' @param epsg A positive whole number of the epsg to transform features to.
#' @param limit A positive whole number of the maximum number of features to return.
#' @return A sf object
#' @family rkm
#' @export
#' @examples
#' fwa_rkm(356308001)
fwa_rkm <- function(blue_line_key, interval = 1000, distance_upstream = 0, epsg = getOption("fwa.epsg", 3005), limit = getOption("fwa.limit", 1000)){
  chk_whole_number(blue_line_key)
  chk_whole_number(interval)
  chk_whole_number(distance_upstream)
  chk_whole_number(epsg)

  x <- fwa_locate_along_interval(blue_line_key, interval_length = interval, start = distance_upstream, epsg = epsg, limit = limit)
  x$rkm <- x$index*interval/1000
  x$blue_line_key <- as.integer(blue_line_key)
  x[c("blue_line_key", "rkm", "geometry")]
}

nearest_rkm <- function(x, rkm) {
  index <- sf::st_nearest_feature(x, rkm)
  x$blue_line_key <- rkm$blue_line_key[index]
  x$rkm <- rkm$rkm[index]
  x$distance_to_rkm <- sf::st_distance(x, rkm[index,], by_element = TRUE)
  x
}

blue_line_key_nearest_rkm <- function(blue_line_key, x, rkm) {
  if(is.na(blue_line_key)) {
    x <- x[is.na(x$blue_line_key),]
  } else {
    x <- x[!is.na(x$blue_line_key) & x$blue_line_key == blue_line_key,]
    rkm <- rkm[rkm$blue_line_key == blue_line_key,]
  }
  nearest_rkm(x, rkm)
}

#' Nearest Rkm
#'
#' Assigns spatial points to river kilometre with matching blue_line_key.
#' If the blue_line_key is missing then it also assigns the blue_line_key.
#'
#' @param x An sf data frame of spatial points with optional blue_line_key column.
#' @param rkm An sf data frame of spatial point with blue_line_key and rkm columns.
#'
#' @return An updated version of x with column rkm and distance_to_rkm of units "m".
#' @family rkm
#' @export
#' @examples
#' \dontrun{
#' rkm <- fwa_rkm(blue_line_key = 356308001, interval = 1000)
#' fwa_nearest_rkm(rkm[2:4,], rkm[3:5,])
#' }
fwa_nearest_rkm <- function(x, rkm) {
  chk::chk_s3_class(x, "sf")
  chk::chk_s3_class(rkm, "sf")

  if(is.null(x$blue_line_key)) x$blue_line_key <- NA_integer_

  check_data(x, values = list(blue_line_key = c(1L, .Machine$integer.max, NA)))
  check_data(rkm, values = list(blue_line_key = c(1L, .Machine$integer.max),
                                rkm = 1))

  if(!nrow(x)) {
    x$rkm <- numeric(0)
    x$distance_to_rkm <- units::as_units(numeric(0), "m")
    return(x)
  }
  if(!nrow(rkm)) {
    x$rkm <- NA_real_
    x$distance_to_rkm <- units::as_units(NA_real_, "m")
    return(x)
  }
  blue_line_keys <- unique(x$blue_line_key)
  if(length(blue_line_keys) == 1L) {
    return(nearest_rkm(x, rkm))
  }

  x$..fwatlasbc.id <- 1:nrow(x)
  x <- lapply(blue_line_keys, blue_line_key_nearest_rkm, x, rkm)
  x <- do.call("rbind", x)
  x <- x[order(x$..fwatlasbc.id),]
  x$..fwatlasbc.id <- NULL
  x
}
