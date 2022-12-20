filter_points <- function(x, y) {
  y <- y |>
    as_tibble() |>
    dplyr::select("blk", "new_rm")

  x |>
    as_tibble() |>
    dplyr::semi_join(y, by = c("blk", rm = "new_rm")) |>
    sf::st_sf()
}

#' Map View River Meters
#'
#' Maps two alternative stream networks by adding links from each point in x to matching point in y.
#'
#' @param x An sf data frame with unique integer columns blk and rm and integer column new_rm.
#' @param y An sf data frame with unique integer columns blk and rm.
#' @param npoint An indication of the total number of points to plot.
#' @inheritParams fwa_mapview
#' @export
fwa_mapview_rms_to_rms <- function(x, y, layer = NULL, zcol = "rm", legend = FALSE, npoint = 250,
                                   ...) {

  if(!requireNamespace("mapview", quietly = TRUE)) {
    err("Package 'mapview' must be installed.")
  }

  chk_s3_class(x, "sf")
  chk_s3_class(sf::st_geometry(x), "sfc_POINT")
  check_names(x, c("blk", "rm", "new_rm"))
  chk_not_subset(colnames(x), c("..fwa_n", "..fwa_p", "..fwa_np"))

  chk_s3_class(y, "sf")
  chk_s3_class(sf::st_geometry(x), "sfc_POINT")
  check_names(x, c("blk", "rm"))

  chk_null_or(zcol, vld = vld_string)
  chk_null_or(layer, vld = vld_string)
  chk_flag(legend)
  chk_whole_number(npoint)
  chk_gt(npoint)

  if(!is.null(layer)) {
    check_names(x, layer)
  }
  if(!is.null(zcol)) {
    check_names(x, zcol)
  }

  chk_whole_numeric(x$blk)
  chk_not_any_na(x$blk)
  chk_gt(x$blk)

  chk_whole_numeric(x$rm)
  chk_not_any_na(x$rm)
  chk_gte(x$rm)

  chk_whole_numeric(x$new_rm)
  chk_gte(x$new_rm)

  check_key(x, c("blk", "rm"))

  chk_whole_numeric(y$blk)
  chk_not_any_na(y$blk)
  chk_gt(y$blk)

  chk_whole_numeric(y$rm)
  chk_not_any_na(y$rm)
  chk_gte(y$rm)

  check_key(y, c("blk", "rm"))

  x <- thin_points(x, npoint)

  y <- filter_points(y, x)

#  z <- make_lines(x, y) need to do this and add and figure out how to have as separate layers!

  fwa_mapview(x, layer = layer, zcol = zcol, legend = legend, ...) +
    fwa_mapview(y, layer = layer, zcol = zcol, legend = legend, alpha = 0, ...)
}
