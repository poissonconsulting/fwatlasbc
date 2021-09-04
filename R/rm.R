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
  chk_null_or(end, chk_whole_number)

  chk_gt(interval)
  chk_gte(start)
  chk_null_or(end, chk_gt, start)

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

nearest_rm <- function(x, rm) {
  blk <- x$blue_line_key
  index <- sf::st_nearest_feature(x, rm)
  x$blue_line_key <- rm$blue_line_key[index]
  x$blue_line_key[is.na(x$blue_line_key)] <- blk[is.na(x$blue_line_key)]
  x$rm <- rm$rm[index]
  x$distance_to_rm <- sf::st_distance(x, rm[index,], by_element = TRUE)
  x$distance_to_rm <- as.numeric(x$distance_to_rm)
  x
}

blue_line_key_nearest_rm <- function(blue_line_key, x, rm) {
  if(is.na(blue_line_key)) {
    x <- x[is.na(x$blue_line_key),]
  } else {
    x <- x[!is.na(x$blue_line_key) & x$blue_line_key == blue_line_key,]
    rm <- rm[rm$blue_line_key == blue_line_key,]
  }
  nearest_rm(x, rm)
}

#' Nearest River Metre
#'
#' Assigns spatial points to river metre with matching blue_line_key.
#' If the blue_line_key is missing then it also assigns the blue_line_key.
#'
#' @param x An sf data frame of spatial points with optional blue_line_key column.
#' @param rm An sf data frame of spatial point with blue_line_key and rm columns.
#'
#' @return An updated version of x with column rm and distance_to_rm in m.
#' @family rm
#' @export
#' @examples
#' \dontrun{
#' rm <- fwa_rm(blue_line_key = 356308001, interval = 1000)
#' fwa_nearest_rm(rm[2:4,], rm[3:5,])
#' }
fwa_nearest_rm <- function(x, rm) {
  chk::chk_s3_class(x, "sf")
  chk::chk_s3_class(rm, "sf")

  if(is.null(x$blue_line_key)) x$blue_line_key <- NA_integer_

  check_data(x, values = list(blue_line_key = c(1L, .Machine$integer.max, NA)))
  check_data(rm, values = list(blue_line_key = c(1L, .Machine$integer.max),
                               rm = 1L))

  if(!nrow(x)) {
    x$rm <- integer(0)
    x$distance_to_rm <- numeric(0)
    return(x)
  }
  if(!nrow(rm)) {
    x$rm <- NA_integer_
    x$distance_to_rm <- NA_real_
    return(x)
  }
  blue_line_keys <- unique(x$blue_line_key)
  if(length(blue_line_keys) == 1L) {
    if(!is.na(blue_line_keys))
      rm <- rm[rm$blue_line_key == blue_line_keys,]
    return(nearest_rm(x, rm))
  }

  x$..fwatlasbc.id <- 1:nrow(x)
  x <- lapply(blue_line_keys, blue_line_key_nearest_rm, x, rm)
  x <- do.call("rbind", x)
  x <- x[order(x$..fwatlasbc.id),]
  x$..fwatlasbc.id <- NULL
  x
}

nearest_feature_rm <- function (rm, y, id, max_distance, max_end_distance) {
  max_distance <- units::as_units(max_distance, "m")
  max_end_distance <- units::as_units(max_end_distance, "m")

  index <- sf::st_nearest_feature(rm, y)
  rm[[id]] <- y[[id]][index]
  distance <- sf::st_distance(rm, y[index,], by_element = TRUE)
  rm[[id]][distance > max_distance] <- NA_integer_
  if(max_end_distance < max_distance) {
    within <- distance <= max_end_distance
    if(any(within)) {
      end <- max(rm$rm[distance <= max_end_distance])
      rm[[id]][rm$rm > end] <- NA_integer_
    }
  }
  rm
}

blue_line_key_nearest_feature_rm <- function(blue_line_key, rm, y, id, max_distance,
                                          max_end_distance) {
  if(is.na(blue_line_key)) {
    rm <- rm[is.na(rm$blue_line_key),]
    max_end_distance <- Inf
  } else {
    rm <- rm[!is.na(rm$blue_line_key) & rm$blue_line_key == blue_line_key,]
    y <- y[y$blue_line_key == blue_line_key,]
  }
  if(!nrow(y)) return(rm)
  nearest_feature_rm(rm, y, id = id, max_distance = max_distance,
                  max_end_distance)
}

#' Add Nearest ID to Rkm
#'
#' Adds id to simple features rm data frame based on
#' nearest feature in sf data frame and blue_line_key if present.
#' Rows which are further than the maximum distance from any feature are assigned a missing value.
#' Missing values are permitted for the blue_line_key for x but not y (if present).
#'
#' @param rm A sf data frame with sfc and rm column and optionally blue_line_key
#' @param y A sf data frame with sfc and id column and optionally blue_line_key.
#' @param id A string of the name of the id column in y.
#' @param max_distance A number of the maximum distance in m to allow a match.
#' @param max_end_distance A number of the maximum distance in m to allow a match
#' for the end rm for each blue_line_key. Applied recursively to trim the ends.
#' @return An copy of rm with additional id column from y.
#' @export
fwa_add_nearest_id_to_rm <- function(rm, y, id = "id", max_distance = 10,
                                     max_end_distance = Inf) {
  chk_s3_class(rm, "sf")
  chk_s3_class(y, "sf")
  chk_string(id)
  chk_number(max_distance)
  chk_number(max_end_distance)

  check_dim(id, nchar, TRUE)
  chk_gt(max_distance)
  chk_gt(max_end_distance)
  check_names(y, id)

  if(!is.null(y$blue_line_key)) {
    check_data(y, values = list(blue_line_key = c(1L, .Machine$integer.max)))
    if(is.null(rm$blue_line_key)) {
      rm$blue_line_key <- NA_integer_
    }
    check_data(rm, values = list(blue_line_key = c(NA, 1L, .Machine$integer.max),
                                 rm = 1L))

  }
  if(!nrow(rm)) {
    rm[[id]] <- y[[id]][0]
    return(rm)
  }
  na <- y[[id]][1]
  is.na(na) <- TRUE
  rm[[id]] <- na
  if(!nrow(y)) {
    return(rm)
  }
  if(is.null(y$blue_line_key)) {
    return(nearest_feature_rm(rm, y, id = id, max_distance = max_distance, max_end_distance = Inf))
  }
  blue_line_keys <- unique(rm$blue_line_key)
  rm$..fwatlasbc.id <- 1:nrow(rm)
  rm <- lapply(blue_line_keys, blue_line_key_nearest_feature_rm, rm, y, id = id, max_distance = max_distance,
               max_end_distance = max_end_distance)
  rm <- do.call("rbind", rm)
  rm <- rm[order(rm$..fwatlasbc.id),]
  rm$..fwatlasbc.id <- NULL
  rm
}
