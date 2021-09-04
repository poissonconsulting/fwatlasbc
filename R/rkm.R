#' Get Rkm
#'
#' Provided a blue_line_key and start and possibly end distance upstream from
#' river mouth, return river kilometre points at regular intervals.
#'
#' @param blue_line_key An integer of the stream blue line key.
#' @param interval An integer of the interval distance in meters.
#' @param start An integer of the distance in meters upstream from the river mouth to start from.
#' @param end An integer of the distance in meters upstream from the river mouth to start from.
#' @param epsg A positive whole number of the epsg to transform features to.
#' @param limit A positive whole number of the maximum number of features to return.
#' @return A sf object
#' @family rkm
#' @export
#' @examples
#' fwa_rkm(356308001)
fwa_rkm <- function(blue_line_key, interval = 1000, start = 0,
                    end = NULL,
                    epsg = getOption("fwa.epsg", 3005),
                    limit = getOption("fwa.limit", 10000)){

  lifecycle::deprecate_soft("0.0.0.9004", "fwa_rkm()", "fwa_rm()")

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
  x$rkm <- (x$index * interval + start)/1000

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

  lifecycle::deprecate_soft("0.0.0.9004", "fwa_nearest_rkm()", "fwa_nearest_rm()")

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
    if(!is.na(blue_line_keys))
      rkm <- rkm[rkm$blue_line_key == blue_line_keys,]
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
#' `r lifecycle::badge('soft-deprecated')` for \code{\link{fwa_add_end_id_to_rkm}()}
#' which adds one column at a time.
#'
#' @param rkm A data frame with columns blue_line_key and rkm.
#' @param y A data frame with columns blue_line_key, rkm and additional columns
#' to add to rkm.
#'
#'
#'
#' @return An ordered version of rkm with additional columns from y.
#' @export
fwa_add_columns_to_rkm <- function(rkm, y) {

  lifecycle::deprecate_soft("0.0.0.9001", "fwa_add_columns_to_rkm()",
                            "fwa_add_end_id_to_rkm()")

  check_data(rkm, values = list(blue_line_key = c(1L, .Machine$integer.max),
                                rkm = 1), key = c("blue_line_key", "rkm"))
  check_data(y, values = list(blue_line_key = c(1L, .Machine$integer.max),
                              rkm = 1), key = c("blue_line_key", "rkm"))

  rkm <- dplyr::as_tibble(rkm)
  y <- dplyr::as_tibble(y)

  rkm <- dplyr::arrange(rkm, .data$blue_line_key, .data$rkm)

  colnames <- colnames(y)
  colnames <- colnames[!colnames %in% c("blue_line_key", "rkm")]

  if(!length(colnames)) return(rkm)

  rkm <- rkm[!colnames(rkm) %in% colnames]

  if(!nrow(rkm)) {
    y <- dplyr::select(y, colnames)
    y <- dplyr::slice(y, 0)
    rkm <- dplyr::bind_cols(rkm, y)
    return(rkm)
  }
  y <- y[y$blue_line_key %in% rkm$blue_line_key,,drop = FALSE]
  if(!nrow(y)) {
    y <- dplyr::select(y, colnames)
    y <- lapply(y, function(x) {is.na(x) <- TRUE; x})
    y <- dplyr::as_tibble(y)
    rkm <- dplyr::bind_cols(rkm, y)
    return(rkm)
  }

  y <- dplyr::arrange(y, .data$blue_line_key, .data$rkm)

  y_max <- dplyr::group_by(y, .data$blue_line_key)
  y_max <- dplyr::summarise(y_max,
                            blue_line_key = dplyr::first(.data$blue_line_key),
                            .fwatlasbc.y.max.. = max(.data$rkm))
  y_max <- dplyr::ungroup(y_max)

  rkm <- dplyr::left_join(rkm, y_max, by = "blue_line_key")
  out <- is.na(rkm$.fwatlasbc.y.max..) | rkm$.fwatlasbc.y.max.. < rkm$rkm
  rkm <- dplyr::select(rkm, -.data$.fwatlasbc.y.max..)
  rkm_out <- dplyr::filter(rkm, out)
  rkm <- dplyr::filter(rkm, !out)

  rkm_out <- fwa_add_columns_to_rkm(rkm_out, y[0,])

  y <- dplyr::mutate(y, .fwatlasbc.y.rkm.. = .data$rkm)
  y <- dplyr::select(y, -.data$rkm)
  rkm <- dplyr::left_join(rkm, y, by = "blue_line_key")
  rkm <- dplyr::mutate(rkm, .fwatlasbc.y.rkm.. = .data$.fwatlasbc.y.rkm.. - .data$rkm)
  rkm <- dplyr::filter(rkm, .data$.fwatlasbc.y.rkm.. >= 0)
  rkm <- dplyr::group_by(rkm, .data$blue_line_key, .data$rkm)
  rkm <- dplyr::slice_min(rkm, .data$.fwatlasbc.y.rkm.., with_ties = FALSE)
  rkm <- dplyr::ungroup(rkm)
  rkm <- dplyr::select(rkm, -.data$.fwatlasbc.y.rkm..)

  rkm <- dplyr::bind_rows(rkm, rkm_out)
  rkm <- dplyr::arrange(rkm, .data$blue_line_key, .data$rkm)
  rkm
}

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
