#' Add Cut to River Meter
#'
#' Adds value(s) in cut column in y to to column of same name in x based on blk and rm_start and rm_end in y.
#'
#' @param x A data frame with integer columns blk and rm.
#' @param y A data frame with integer columns blk, rm_start, rm_end and column specified in cut.
#' @param cut A string of the name of the column in y with the values.
#' @return A tibble of x with cut column from y.
#' @export
fwa_add_cut_to_rms <- function(x, y, cut = "cut") {
  chk_data(x)
  chk_data(y)
  chk_string(cut)
  chk_not_subset(cut, c("blk", "rm", "rm_start", "rm_end"))
  check_names(x, c("blk", "rm"))
  check_names(y, c("blk", "rm_start", "rm_end", cut))
  chk_not_subset(colnames(x), cut)

  chk_whole_numeric(x$blk)
  chk_not_any_na(x$blk)
  chk_gt(x$blk)
  chk_whole_numeric(x$rm)
  chk_not_any_na(x$rm)
  chk_gte(x$rm)
  chk_whole_numeric(x$blk)

  chk_whole_numeric(y$blk)
  chk_not_any_na(y$blk)
  chk_gt(y$blk)
  chk_whole_numeric(y$rm_start)
  chk_not_any_na(y$rm_start)
  chk_gte(y$rm_start)
  chk_whole_numeric(y$rm_end)
  chk_not_any_na(y$rm_end)
  chk_gte(y$rm_end)

  sf_column_name <- sf_column_name(x)

  x <- x |>
    dplyr::as_tibble()

  if (!nrow(x)) {
    x[[cut]] <- y[[cut]][0]
    return(x)
  }

  y <- y |>
    dplyr::as_tibble() |>
    dplyr::filter(.data$blk %in% x$blk)

  x[[cut]] <- y[[cut]][0][1] # gets missing value
  if (!nrow(y)) {
    return(x)
  }

  for (i in seq_len(nrow(y))) {
    min_rm <- min(y$rm_start[i], y$rm_end[i])
    max_rm <- max(y$rm_start[i], y$rm_end[i])
    x[[cut]][x$blk == y$blk[i] & x$rm >= min_rm & x$rm <= max_rm] <- y[[cut]][i]
  }
  if (!identical(sf_column_name, character(0))) {
    x <- x |>
      sf::st_sf(sf_column_name = sf_column_name)
  }
  x
}
