filter_points <- function(x, y) {
  y <- y |>
    as_tibble() |>
    dplyr::select("blk", "new_rm")

  x |>
    as_tibble() |>
    dplyr::semi_join(y, by = c("blk", rm = "new_rm")) |>
    sf::st_sf()
}

join_points <- function(x, y) {
  crs <- sf::st_crs(x)
  sf::st_crs(y) <- crs

  x <- x |>
    as_tibble() |>
    dplyr::rename(..fwa_geometry1 = "geometry")
  y <- y |>
    as_tibble() |>
    dplyr::rename(..fwa_geometry2 = "geometry") |>
    dplyr::select("blk", "rm", "..fwa_geometry2")
  z <- x |>
    dplyr::inner_join(y, by = c("blk" = "blk", "new_rm" = "rm"))

  z$geometry <- sf::st_sfc(mapply(function(a, b) {
    sf::st_cast(sf::st_union(a, b), "LINESTRING")
  }, z$..fwa_geometry1, z$..fwa_geometry2, SIMPLIFY = FALSE), crs = crs)

  z |>
    dplyr::select(-"..fwa_geometry1", -"..fwa_geometry2") |>
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
fwa_mapview_rms_to_rms <- function(x, y, zcol = "rm", npoint = 250) {
  rlang::check_installed("mapview")

  chk_s3_class(x, "sf")
  chk_s3_class(sf::st_geometry(x), "sfc_POINT")
  check_names(x, c("blk", "rm", "new_rm"))
  chk_not_subset(colnames(x), c("..fwa_n", "..fwa_p", "..fwa_np"))

  chk_s3_class(y, "sf")
  chk_s3_class(sf::st_geometry(x), "sfc_POINT")
  check_names(x, c("blk", "rm"))

  chk_null_or(zcol, vld = vld_string)
  chk_whole_number(npoint)
  chk_gt(npoint)

  if (!is.null(zcol)) {
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

  z <- join_points(x, y)

  mapview::mapview(x, zcol = zcol, legend = FALSE) +
    mapview::mapview(y, zcol = zcol, alpha = 0, legend = FALSE) +
    mapview::mapview(z, zcol = zcol, legend = FALSE)
}
