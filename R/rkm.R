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
fwa_rkm <- function(blue_line_key, interval = 1000, distance_upstream = 0,
                    epsg = getOption("fwa.epsg", 3005),
                    limit = getOption("fwa.limit", 10000)){
  chk_whole_number(blue_line_key)
  chk_whole_number(interval)
  chk_whole_number(distance_upstream)
  chk_whole_number(epsg)

  x <- fwa_locate_along_interval(blue_line_key,
                                 interval_length = interval,
                                 start = distance_upstream,
                                 epsg = epsg,
                                 limit = limit)
x$rkm <- (x$index*interval + distance_upstream)/1000

  x$blue_line_key <- as.integer(blue_line_key)
  x[c("blue_line_key", "rkm", "geometry")]
}

nearest_rkm <- function(x, rkm) {
  blk <- x$blue_line_key
  index <- sf::st_nearest_feature(x, rkm)
  x$blue_line_key <- rkm$blue_line_key[index]
  x$blue_line_key[is.na(x$blue_line_key)] <- blk[is.na(x$blue_line_key)]
  x$rkm <- rkm$rkm[index]
  x$distance_to_rkm <- sf::st_distance(x, rkm[index,], by_element = TRUE)
  x$distance_to_rkm <- as.numeric(x$distance_to_rkm)
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
#' @return An updated version of x with column rkm and distance_to_rkm in m.
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
    x$distance_to_rkm <- numeric(0)
    return(x)
  }
  if(!nrow(rkm)) {
    x$rkm <- NA_real_
    x$distance_to_rkm <- NA_real_
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

#' Add Columns to Rkm
#'
#' Adds columns to rkm based on end river kilometers in y.
#'
#' @param rkm A data frame with columns blue_line_key and rkm.
#' @param y A data frame with columns blue_line_key, rkm and additional columns
#' to add to rkm.
#'
#' @return An ordered version of rkm with additional columns from y.
#' @export
fwa_add_columns_to_rkm <- function(rkm, y) {
  check_data(rkm, values = list(blue_line_key = c(1L, .Machine$integer.max),
                                rkm = 1), key = c("blue_line_key", "rkm"))
  check_data(y, values = list(blue_line_key = c(1L, .Machine$integer.max),
                              rkm = 1), key = c("blue_line_key", "rkm"))

  rkm <- as.data.frame(rkm, stringsAsFactors = FALSE)
  y <- as.data.frame(y, stringsAsFactors = FALSE)

  rkm <- rkm[order(rkm$blue_line_key, rkm$rkm),]

  colnames <- colnames(y)
  colnames <- colnames[!colnames %in% c("blue_line_key", "rkm")]

  if(!length(colnames)) return(rkm)

  rkm <- rkm[!colnames(rkm) %in% colnames]

  if(!nrow(rkm)) {
    y <- y[0, colnames, drop = FALSE]
    rkm <- cbind(rkm, y)
    return(rkm)
  }
  y <- y[y$blue_line_key %in% rkm$blue_line_key,,drop = FALSE]
  if(!nrow(y)) {
    y <- y[colnames]
    y <- lapply(y, function(x) {is.na(x) <- TRUE; x})
    y <- as.data.frame(y)
    rkm <- cbind(rkm, y)
    return(rkm)
  }

  y <- y[order(y$blue_line_key, y$rkm),,drop = FALSE]

  y_max <- split(y, y$blue_line_key)
  y_max <- lapply(y_max, function(x) data.frame(
    blue_line_key = x$blue_line_key[1],
    .fwatlasbc.y.max.. = max(x$rkm)))
  y_max <- do.call("rbind", y_max)

  rkm <- merge(rkm, y_max, by = "blue_line_key", all.x = TRUE)
  out <- is.na(rkm$.fwatlasbc.y.max..) | rkm$.fwatlasbc.y.max.. < rkm$rkm
  rkm$.fwatlasbc.y.max..<- NULL
  rkm_out <- rkm[out,,drop = FALSE]
  rkm <- rkm[!out,,drop = FALSE]

  rkm_out <- fwa_add_columns_to_rkm(rkm_out, y[0,])

  y$.fwatlasbc.y.rkm.. <- y$rkm
  y$rkm <- NULL
  rkm <- merge(rkm, y, by = "blue_line_key", all.x = TRUE)
  rkm$.fwatlasbc.y.rkm.. <- rkm$.fwatlasbc.y.rkm.. - rkm$rkm
  rkm <- rkm[rkm$.fwatlasbc.y.rkm.. >= 0,, drop = FALSE]
  rkm <- split(rkm, list(rkm$blue_line_key, rkm$rkm))
  rkm <- lapply(rkm, function(x) x[which.min(x$.fwatlasbc.y.rkm..),,drop = FALSE])
  rkm <- do.call("rbind", rkm)
  rkm$.fwatlasbc.y.rkm.. <- NULL

  rkm <- rbind(rkm, rkm_out)
  rkm <- rkm[order(rkm$blue_line_key, rkm$rkm),,drop = FALSE]
  row.names(rkm) <- NULL
  rkm
}
