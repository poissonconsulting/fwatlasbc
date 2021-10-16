#' Add End ID to River Kilometre
#'
#' Adds id column to rm based on end river metres in y.
#' All rms up to and including the end but before the previous end
#' are assigned the id (which can be a missing value)
#'
#' @param rm A data frame with columns blue_line_key and rm.
#' @param y A data frame with columns blue_line_key, rm and and id column to add.
#' @param id A string of the name of the column in y (can include missing values).
#' @return An ordered version of rm with id columns from y.
#' @export
fwa_add_end_id_to_rm <- function(rm, y, id = "id") {

  check_data(rm, values = list(blue_line_key = c(1L, .Machine$integer.max),
                                rm = 1L), key = c("blue_line_key", "rm"))
  check_data(y, values = list(blue_line_key = c(1L, .Machine$integer.max),
                              rm = 1L), key = c("blue_line_key", "rm"))
  check_key(y, c("blue_line_key", "rm"))
  chk_string(id)
  check_dim(id, nchar, TRUE)
  chk_not_subset(id, c("blue_line_key", "rm"))
  check_names(y, id)

  rm <- dplyr::as_tibble(rm)
  y <- dplyr::as_tibble(y)

  y <- y[c("blue_line_key", "rm", id)]

  if(!is.null(rm[[id]])) rm[[id]] <- NULL

  rm <- dplyr::arrange(rm, .data$blue_line_key, .data$rm)

  if(!nrow(rm)) {
    rm[[id]] <- y[[id]][0]
    return(rm)
  }

  y <- y[y$blue_line_key %in% rm$blue_line_key,,drop = FALSE]
  if(!nrow(y)) {
    rm[[id]] <- y[[id]][1]
    return(rm)
  }

  y <- dplyr::arrange(y, .data$blue_line_key, .data$rm)

  y_max <- dplyr::group_by(y, .data$blue_line_key)
  y_max <- dplyr::summarise(y_max,
                            blue_line_key = dplyr::first(.data$blue_line_key),
                            .fwatlasbc.y.max.. = max(.data$rm))
  y_max <- dplyr::ungroup(y_max)

  rm <- dplyr::left_join(rm, y_max, by = "blue_line_key")
  out <- is.na(rm$.fwatlasbc.y.max..) | rm$.fwatlasbc.y.max.. < rm$rm
  rm <- dplyr::select(rm, -.data$.fwatlasbc.y.max..)
  rm_out <- dplyr::filter(rm, out)
  rm <- dplyr::filter(rm, !out)

  rm_out <- fwa_add_end_id_to_rm(rm_out, y[0,], id = id)

  y <- dplyr::mutate(y, .fwatlasbc.y.rm.. = .data$rm)
  y <- dplyr::select(y, -.data$rm)

  rm <- dplyr::left_join(rm, y, by = "blue_line_key")
  rm <- dplyr::mutate(rm, .fwatlasbc.y.rm.. = .data$.fwatlasbc.y.rm.. - .data$rm)
  rm <- dplyr::filter(rm, .data$.fwatlasbc.y.rm.. >= 0)
  rm <- dplyr::group_by(rm, .data$blue_line_key, .data$rm)
  rm <- dplyr::slice_min(rm, .data$.fwatlasbc.y.rm.., with_ties = FALSE)
  rm <- dplyr::ungroup(rm)
  rm <- dplyr::select(rm, -.data$.fwatlasbc.y.rm..)

  rm <- dplyr::bind_rows(rm, rm_out)
  rm <- dplyr::arrange(rm, .data$blue_line_key, .data$rm)
  rm
}
