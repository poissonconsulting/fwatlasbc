#' Prune River Meter
#'
#' Removes river meters above rm values in y.
#'
#' @param x A data frame with integer columns blk, rm, parent_blk and parent_rm.
#' @param y A data frame with integer columns blk and rm.
#' @return A tibble of x with rows above the rms in y removed.
#' @export
fwa_prune_rms <- function(x, y) {
  chk_data(x)
  chk_data(y)

  check_names(x, c("blk", "rm", "parent_blk", "parent_rm"))
  check_names(y, c("blk", "rm"))
  chk_not_subset(colnames(x), "..fwa_prune")
  y$name <- "..fwa_prune"

  n <- nrow(y)
  for(i in seq_len(n)) {
    z <- y |> dplyr::slice(n)
    x <- x |>
      fwa_add_upstream_split_to_rms(z) |>
      dplyr::filter(!.data$..fwa_prune) |>
      dplyr::select(!"..fwa_prune")
  }
  x
}
