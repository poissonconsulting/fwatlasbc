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
  rm$..fwa_id <- 1:nrow(rm)
  rm <- lapply(blue_line_keys, blue_line_key_nearest_feature_rm, rm, y, id = id, max_distance = max_distance,
               max_end_distance = max_end_distance)
  rm <- do.call("rbind", rm)
  rm <- rm[order(rm$..fwa_id),]
  rm$..fwa_id <- NULL
  rm
}
