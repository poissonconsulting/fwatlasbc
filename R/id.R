nearest_feature <- function (x, y, id, max_distance) {
  index <- sf::st_nearest_feature(x, y)
  x[[id]] <- y[[id]][index]
  distance <- sf::st_distance(x, y[index,], by_element = TRUE)
  x[[id]][distance > max_distance] <- NA_integer_
  x
}

blue_line_key_nearest_feature <- function(blue_line_key, x, y, id, max_distance) {
  if(is.na(blue_line_key)) {
    x <- x[is.na(x$blue_line_key),]
  } else {
    x <- x[!is.na(x$blue_line_key) & x$blue_line_key == blue_line_key,]
    y <- y[y$blue_line_key == blue_line_key,]
  }
  nearest_feature(x, y, id = id, max_distance = max_distance)
}


#' Add Nearest ID
#'
#' Adds positive integer id to simple features data frame based on
#' nearest feature in sf data frame and blue_line_key if present.
#' Rows which are further than the maximum distance from any feature are assigned a missing value.
#' Useful for assigning line features to rkm table.
#' Missing values are permitted for the blue_line_key for x but not y.
#'
#' @param x A sf data frame with sfc column and optionally blue_line_key
#' @param y A sf data frame with sfc and id column and optionally blue_line_key.
#' @param id A string of the name of the positive integer column in y.
#' @param max_distance A number of the maximum distance to allow a match.
#' @return An copy of rkm with additional id column from y.
#' @export
fwa_add_nearest_id <- function(x, y, id = "id", max_distance = 10) {
  chk_s3_class(x, "sf")
  chk_s3_class(y, "sf")
  chk_string(id)
  chk_number(max_distance)

  check_dim(id, nchar, TRUE)
  chk_gt(max_distance)

  check_data(y, values = stats::setNames(list(c(1L, .Machine$integer.max)), id))

  if(!is.null(y$blue_line_key)) {
    check_data(y, values = list(blue_line_key = c(1L, .Machine$integer.max)))
    if(is.null(x$blue_line_key)) {
      x$blue_line_key <- NA_integer_
    }
    check_data(x, values = list(blue_line_key = c(NA, 1L, .Machine$integer.max)))

  }
  if(!nrow(x)) {
    x[[id]] <- integer(0)
    return(x)
  }
  x[[id]] <- NA_integer_
  if(!nrow(y)) {
    return(x)
  }
  max_distance <- units::as_units(max_distance, "m")
  if(is.null(y$blue_line_key)) {
    return(nearest_feature(x, y, id = id, max_distance = max_distance))
  }
  blue_line_keys <- unique(x$blue_line_key)
  blue_line_keys <- blue_line_keys[is.na(blue_line_keys) | blue_line_keys %in% y$blue_line_key]
  x$..fwatlasbc.id <- 1:nrow(x)
  x <- lapply(blue_line_keys, blue_line_key_nearest_feature, x, y, id = id, max_distance = max_distance)
  x <- do.call("rbind", x)
  x <- x[order(x$..fwatlasbc.id),]
  x$..fwatlasbc.id <- NULL
  x
}
