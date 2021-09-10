#' Add End ID to Rkm
#'
#' Adds id column to rkm based on end river kilometers in y.
#' All rkms up to and including the end but before the previous end
#' are assigned the id (which can be a missing value)
#'
#' @param rkm A data frame with columns blue_line_key and rkm.
#' @param y A data frame with columns blue_line_key, rkm and and id column to add.
#' @param id A string of the name of the column in y (can include missing values).
#' @return An ordered version of rkm with id columns from y.
#' @export
fwa_add_end_id_to_rkm <- function(rkm, y, id = "id") {
  lifecycle::deprecate_soft("0.0.0.9004", "fwa_add_end_id_to_rkm()", "fwa_add_end_id_to_rm()")

  check_data(rkm, values = list(blue_line_key = c(1L, .Machine$integer.max),
                                rkm = 1), key = c("blue_line_key", "rkm"))
  check_data(y, values = list(blue_line_key = c(1L, .Machine$integer.max),
                              rkm = 1), key = c("blue_line_key", "rkm"))
  check_key(y, c("blue_line_key", "rkm"))
  chk_string(id)
  check_dim(id, nchar, TRUE)
  chk_not_subset(id, c("blue_line_key", "rkm"))
  check_names(y, id)

  rkm <- dplyr::as_tibble(rkm)
  y <- dplyr::as_tibble(y)

  y <- y[c("blue_line_key", "rkm", id)]

  if(!is.null(rkm[[id]])) rkm[[id]] <- NULL

  rkm <- dplyr::arrange(rkm, .data$blue_line_key, .data$rkm)

  if(!nrow(rkm)) {
    rkm[[id]] <- y[[id]][0]
    return(rkm)
  }

  y <- y[y$blue_line_key %in% rkm$blue_line_key,,drop = FALSE]
  if(!nrow(y)) {
    rkm[[id]] <- y[[id]][1]
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

  rkm_out <- fwa_add_end_id_to_rkm(rkm_out, y[0,], id = id)

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
