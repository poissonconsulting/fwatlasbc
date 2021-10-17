add_watershed_to_blk <- function(x, epsg) {
  check_dim(x, dim = nrow, values = 1L) # +chk

  wshed <- try(fwa_watershed_at_measure(blue_line_key = x$BLK,
                                        downstream_route_measure = x$StartRM,
                                        epsg = epsg), silent = TRUE)
  if(is_try_error(wshed))
    abort_chk("Unable to retrieve watershed for BLK ", x$BLK, " with start ",
              x$StartRM, ".")

  # includes
  wshed <- wshed |>
    dplyr::select(Area = .data$area_ha, .data$geometry)

  x |>
    dplyr::bind_cols(wshed) |>
    sf::st_set_geometry("geometry")
}
#' Add Watershed to Blue Line Key
#'
#' Adds watershed area (Area) and polygon (geometry) to blue line key (BLK).
#' The start distances which is in meters is from the river mouth.
#' @return An sf tibble with the columns of x plus integer column RM,
#' numeric column Area and sf column geometry.
#'
#' @inheritParams fwapgr::fwa_locate_along
#' @param start A positive whole numeric of the distance in meters upstream
#' from the river mouth.
#' @return A sf object
#' @seealso \code{\link[fwapgr]{fwa_watershed_at_measure}}.
#' @export
#' @examples
#' \dontrun{
#' fwa_add_watershed_to_blk(data.frame(BLK = 356308001))
#' }
fwa_add_watershed_to_blk <- function(x,
                                     start = 0,
                                     epsg = getOption("fwa.epsg", 3005)) {
  check_data(x)
  check_dim(x, dim = nrow, values = TRUE)
  chk_whole_numeric(x$BLK)
  chk_subset(x$BLK, unique(named_streams$BLK))
  chk_unique(x$BLK)
  chk_not_subset(colnames(x), c("StartRM", "Area", "geometry"))
  chk_whole_number(start)
  chk_gte(start)

  x |>
    dplyr::as_tibble() |>
    dplyr::mutate(StartRM = start) |>
    dplyr::group_split(.data$BLK) |>
    lapply(add_watershed_to_blk, epsg = epsg) |>
    dplyr::bind_rows()
}
