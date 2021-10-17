add_watershed_fundamental_to_blk <- function(x, epsg) {
  check_dim(x, dim = nrow, values = 1L) # +chk

  wshed <- try(fwa_watershed_hex(blue_line_key = x$BLK,
                                 downstream_route_measure = x$..fwa_rm,
                                 epsg = epsg), silent = TRUE)
  if(is_try_error(wshed)) {
    abort_chk("Unable to retrieve fundamental watershed for BLK ", x$BLK, " at rm ",
              x$..fwa_rm, ".")
  }

  x |>
    dplyr::mutate(geometry = sf::st_union(wshed$geometry)) |>
    sf::st_set_geometry("geometry")
}
#' Add Fundamental Watershed to Blue Line Key
#'
#' Adds sfc polygon (geometry) of fundamental watershed to blue line key (BLK).
#' @return An sf tibble with the columns of x plus
#' sfc polygon column geometry.
#'
#' @inheritParams fwapgr::fwa_locate_along
#' @param rm A positive whole numeric of the distance in meters upstream
#' from the river mouth.
#' @return A sf object
#' @seealso \code{\link[fwapgr]{fwa_watershed_at_measure}}.
#' @export
#' @examples
#' \dontrun{
#' fwa_add_watershed_fundamental_to_blk(data.frame(BLK = 360879896L), rm = 100)
#' }
fwa_add_watershed_fundamental_to_blk <- function(x, rm = 0,
                                                 epsg = getOption("fwa.epsg", 3005)) {
  check_data(x)
  check_dim(x, dim = nrow, values = TRUE)
  chk_whole_numeric(x$BLK)
  chk_not_any_na(x$BLK)
  chk_subset(x$BLK, unique(named_streams$BLK))
  chk_unique(x$BLK)
  chk_not_subset(colnames(x), "geometry")
  chk_not_subset(colnames(x), "..fwa_rm")
  chk_whole_numeric(rm)
  chk_not_any_na(rm)
  chk_gte(rm)
  chk_whole_number(epsg)
  chk_gte(epsg)

  x |>
    dplyr::as_tibble() |>
    dplyr::mutate(..fwa_rm = rm) |>
    dplyr::group_split(.data$BLK) |>
    lapply(add_watershed_fundamental_to_blk, epsg = epsg) |>
    dplyr::bind_rows() |>
    dplyr::select(-.data$..fwa_rm)
}
