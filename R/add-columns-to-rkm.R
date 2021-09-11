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
    y <- dplyr::select(y, dplyr::all_of(colnames))
    y <- dplyr::slice(y, 0)
    rkm <- dplyr::bind_cols(rkm, y)
    return(rkm)
  }
  y <- y[y$blue_line_key %in% rkm$blue_line_key,,drop = FALSE]
  if(!nrow(y)) {
    y <- dplyr::select(y, dplyr::all_of(colnames))
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
