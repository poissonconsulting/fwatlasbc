#' Map View
#'
#' A wrapper on `mapview::mapview.sf()` that allows the user to layer by a column
#' and coerces hms columns to character (to avoid being dropped).
#'
#' @param x An sf data frame.
#' @param zcol A string of the column to color points by.
#' @param layer A string of the column to layer the points by.
#' @param legend A flag specifying whether to plot a legend.
#' @param ... Additional arguments passed to `mapview::mapview()`.
#' @export
fwa_mapview <- function(x, layer = NULL, zcol = NULL, legend = FALSE, ...) {
  if (!requireNamespace("mapview", quietly = TRUE)) {
    err("Package 'mapview' must be installed.")
  }

  if (!requireNamespace("hms", quietly = TRUE)) {
    err("Package 'hms' must be installed.")
  }

  chk_s3_class(x, "sf")

  x <- x |>
    dplyr::mutate(
      dplyr::across(tidyselect::vars_select_helpers$where(hms::is_hms), as.character)
    )

  if (!is.null(layer)) {
    x <- x |>
      split(x[[layer]], drop = TRUE)
  }

  mapview::mapview(x, zcol = zcol, legend = legend, ...)
}
