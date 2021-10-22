split_blk <- function(x, blk, rm = 0) {
  x$..fwa_split[x$blk == blk & x$rm >= rm] <- TRUE

  blks <- x$blk[!is.na(x$parent_blk) & x$parent_blk == blk] |>
    unique()

  for(blk in blks) {
    x <- split_blk(x, blk)
  }
  x
}

get_split_to_rms <- function(name, x, y) {
  y <- y |>
    dplyr::filter(.data$name == name)

  check_dim(y, nrow, 1L) # +chk

  x <- x |>
    dplyr::mutate(..fwa_split = FALSE) |>
    as.environment() |>
    split_blk(blk = y$blk, rm = y$rm)

  x$..fwa_split
}

#' Add Split to River Meter
#'
#' Splits river meters with parent_blk and parent_rm columns
#' into upstream (TRUE) versus not (FALSE).
#'
#' @param x A data frame with integer columns blk, rm, parent_blk and parent_rm.
#' @param y A data frame with integer columns blk and rm and character column name.
#' @return A tibble of x with a logical column for each name in name.
#' @export
fwa_add_split_to_rms <- function(x, y) {
  chk_data(x)
  chk_data(y)

  check_dim(x, nrow)

  check_names(x, c("blk", "rm", "parent_blk", "parent_rm"))
  check_names(y, c("blk", "rm", "name"))
  chk_not_subset(colnames(x), x$names)
  chk_not_subset(colnames(x), "..fwa_split")

  chk_whole_numeric(x$blk)
  chk_not_any_na(x$blk)
  chk_gt(x$blk)

  chk_whole_numeric(x$rm)
  chk_not_any_na(x$rm)
  chk_gte(x$rm)

  chk_whole_numeric(x$parent_blk)
  chk_gt(x$parent_blk)

  chk_whole_numeric(x$parent_rm)
  chk_gte(x$parent_rm)

  chk_whole_numeric(y$blk)
  chk_not_any_na(y$blk)
  chk_gt(y$blk)

  chk_whole_numeric(y$rm)
  chk_not_any_na(y$rm)
  chk_gte(y$rm)

  chk_character_or_factor(y$name)
  chk_not_any_na(y$name)
  chk_unique(y$name)
  chk_valid_name(y$name)

  if(!nrow(y)) return(x)


  splits <- lapply(y$name, get_split_to_rms, x = x, y = y)
  names(splits) <- y$name
  splits <- dplyr::as_tibble(splits)

  if(!"tbl" %in% class(x)) {
    x <- dplyr::as_tibble(x)
  }

  x |>
    dplyr::bind_cols(splits)
}
