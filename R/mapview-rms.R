thin_points <- function(x, npoint) {
  x <- x |>
    dplyr::arrange(.data$blk, .data$rm)

  if(nrow(x) <= npoint) {
    return(x)
  }

  dplyr::as_tibble(x) |>
    dplyr::group_by(.data$blk) |>
    dplyr::summarise(..fwa_n = n()) |>
    dplyr::ungroup() |>
    dplyr::mutate(..fwa_p = .data$..fwa_n / sum(.data$..fwa_n),
                  ..fwa_np = .data$..fwa_p * npoint,
                  ..fwa_np = ceiling(.data$..fwa_np),
                  ..fwa_np = pmax(.data$..fwa_np, 2L)) |>
    dplyr::inner_join(x, by = "blk") |>
    dplyr::group_by(.data$blk) |>
    dplyr::mutate(..fwa_np = pmin(.data$..fwa_np, .data$..fwa_n)) |>
    dplyr::slice(round_up(seq(1, n(), length.out = .data$..fwa_np[1]))) |>
    dplyr::ungroup() |>
    dplyr::select(-.data$..fwa_n, -.data$..fwa_p, -.data$..fwa_np) |>
    sf::st_as_sf()
}

#' Map View River Meters
#'
#' @param x An sf data frame with unique integer columns blk and rm.
#' @param zcol A string of the column to color points by.
#' @param npoint An indication of the total number of points to plot.
#' @export
fwa_mapview_rms <- function(x, zcol = "rm", npoint = 500) {

  if(!requireNamespace("mapview", quietly = TRUE)) {
    err("Package 'mapview' must be installed.")
  }

  chk_s3_class(x, "sf")
  chk_s3_class(sf::st_geometry(x), "sfc_POINT")
  check_names(x, c("blk", "rm"))
  chk_not_subset(colnames(x), c("..fwa_n", "..fwa_p", "..fwa_np"))
  chk_string(zcol)
  chk_whole_number(npoint)
  chk_gt(npoint)


  chk_whole_numeric(x$blk)
  chk_not_any_na(x$blk)
  chk_gt(x$blk)

  chk_whole_numeric(x$rm)
  chk_not_any_na(x$rm)
  chk_gte(x$rm)

  check_key(x, c("blk", "rm"))

  x <- thin_points(x, npoint)
  mapview::mapview(x, zcol = zcol)
}
