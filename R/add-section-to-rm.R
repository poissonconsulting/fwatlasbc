#' Add Section to River Meter
#'
#' Adds section column in y to x based on BLK and end rm in y.
#' All rms in x up to and including the end rm but before the previous end
#' rm are assigned the section column value (which can be missing).
#'
#' @param x A data frame with integer columns BLK and rm.
#' @param y A data frame with integer columns BLK and rm and column specified in section.
#' @param section A string of the name of the column in y (can include missing values).
#' @return A tibble of x with section column from y.
#' @export
fwa_add_section_to_rm <- function(x, y, section = "Section") {
  chk_data(x)
  chk_data(y)
  chk_string(section)
  chk_not_subset(section, c("BLK", "rm"))
  check_names(x, c("BLK", "rm"))
  check_names(y, c("BLK", "rm", section))
  chk_not_subset(colnames(x), section)

  chk_whole_numeric(x$BLK)
  chk_not_any_na(x$BLK)
  chk_gt(x$BLK)
  chk_whole_numeric(x$rm)
  chk_not_any_na(x$rm)
  chk_gte(x$rm)
  chk_whole_numeric(x$BLK)

  chk_whole_numeric(y$BLK)
  chk_not_any_na(y$BLK)
  chk_gt(y$BLK)
  chk_whole_numeric(y$rm)
  chk_not_any_na(y$rm)
  chk_gte(y$rm)

  check_key(y, c("BLK", "rm"))

  x <- x |>
    dplyr::as_tibble()

  if(!nrow(x)) {
    x[[section]] <- y[[section]][0]
    return(x)
  }

  y <- y |>
    dplyr::as_tibble() |>
    dplyr::arrange(.data$BLK, dplyr::desc(.data$rm)) |>
    dplyr::filter(.data$BLK %in% x$BLK)

  x[[section]] <- y[[section]][0][1] # gets missing value
  if(!nrow(y)) {
    return(x)
  }

  for(i in seq_len(nrow(y))) {
    x[[section]][x$BLK == y$BLK[i] & x$rm <= y$rm[i]] <-  y[[section]][i]
  }
  x
}
