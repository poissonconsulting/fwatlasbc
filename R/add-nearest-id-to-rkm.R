
nearest_feature <- function (rkm, y, id, max_distance, max_end_distance) {
  max_distance <- units::as_units(max_distance, "m")
  max_end_distance <- units::as_units(max_end_distance, "m")

  index <- sf::st_nearest_feature(rkm, y)
  rkm[[id]] <- y[[id]][index]
  distance <- sf::st_distance(rkm, y[index,], by_element = TRUE)
  rkm[[id]][distance > max_distance] <- NA_integer_
  if(max_end_distance < max_distance) {
    within <- distance <= max_end_distance
    if(any(within)) {
      end <- max(rkm$rkm[distance <= max_end_distance])
      rkm[[id]][rkm$rkm > end] <- NA_integer_
    }
  }
  rkm
}

blue_line_key_nearest_feature <- function(blue_line_key, rkm, y, id, max_distance,
                                          max_end_distance) {
  if(is.na(blue_line_key)) {
    rkm <- rkm[is.na(rkm$blue_line_key),]
    max_end_distance <- Inf
  } else {
    rkm <- rkm[!is.na(rkm$blue_line_key) & rkm$blue_line_key == blue_line_key,]
    y <- y[y$blue_line_key == blue_line_key,]
  }
  if(!nrow(y)) return(rkm)
  nearest_feature(rkm, y, id = id, max_distance = max_distance,
                  max_end_distance)
}

#' Add Nearest ID to Rkm
#'
#' Adds id to simple features rkm data frame based on
#' nearest feature in sf data frame and blue_line_key if present.
#' Rows which are further than the maximum distance from any feature are assigned a missing value.
#' Missing values are permitted for the blue_line_key for x but not y (if present).
#'
#' @param rkm A sf data frame with sfc and rkm column and optionally blue_line_key
#' @param y A sf data frame with sfc and id column and optionally blue_line_key.
#' @param id A string of the name of the id column in y.
#' @param max_distance A number of the maximum distance in m to allow a match.
#' @param max_end_distance A number of the maximum distance in m to allow a match
#' for the end rkm for each blue_line_key. Applied recursively to trim the ends.
#' @return An copy of rkm with additional id column from y.
#' @export
fwa_add_nearest_id_to_rkm <- function(rkm, y, id = "id", max_distance = 10,
                                      max_end_distance = Inf) {
  lifecycle::deprecate_soft("0.0.0.9004", "fwa_add_nearest_id_to_rkm()", "fwa_add_nearest_id_to_rm()")

  chk_s3_class(rkm, "sf")
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
    if(is.null(rkm$blue_line_key)) {
      rkm$blue_line_key <- NA_integer_
    }
    check_data(rkm, values = list(blue_line_key = c(NA, 1L, .Machine$integer.max),
                                  rkm = 1))

  }
  if(!nrow(rkm)) {
    rkm[[id]] <- y[[id]][0]
    return(rkm)
  }
  na <- y[[id]][1]
  is.na(na) <- TRUE
  rkm[[id]] <- na
  if(!nrow(y)) {
    return(rkm)
  }
  if(is.null(y$blue_line_key)) {
    return(nearest_feature(rkm, y, id = id, max_distance = max_distance, max_end_distance = Inf))
  }
  blue_line_keys <- unique(rkm$blue_line_key)
  rkm$..fwatlasbc.id <- 1:nrow(rkm)
  rkm <- lapply(blue_line_keys, blue_line_key_nearest_feature, rkm, y, id = id, max_distance = max_distance,
                max_end_distance = max_end_distance)
  rkm <- do.call("rbind", rkm)
  rkm <- rkm[order(rkm$..fwatlasbc.id),]
  rkm$..fwatlasbc.id <- NULL
  rkm
}
