#' Add elevatr Elevation to Point
#'
#' Adds elevations from the Amazon Web Services Terrain Tiles using
#' [`elevatr::get_elev_point()`].
#' Unlike [`fwa_add_gm_elevation_to_point()`] no API key is required.
#'
#' @param x An sf object of spatial points.
#' @param zoom A whole number between 1 and 14 of the zoom level of the
#' Terrain Tiles.
#' Higher zoom levels have a finer resolution but take longer to download.
#' @return An updated version of x with numeric column elevation.
#' @seealso [`elevatr::get_elev_point()`] and
#' [`fwa_add_gm_elevation_to_point()`]
#' @export
#' @examples
#' \dontrun{
#' rm <- fwa_add_rms_to_blk(data.frame(blk = 356308001))
#' fwa_add_elevatr_elevation_to_point(rm)
#' }
fwa_add_elevatr_elevation_to_point <- function(x, zoom = 12) {
  # elevatr uses progress at runtime but only suggests it
  rlang::check_installed(
    c("elevatr", "progress"),
    reason = "to get elevations from the Terrain Tiles."
  )

  chk_s3_class(x, "sf")
  chk_s3_class(sf::st_geometry(x), "sfc_POINT")
  chk_whole_number(zoom)
  chk_range(zoom, c(1, 14))

  if (!nrow(x)) {
    x$elevation <- numeric(0)
    return(x)
  }

  points <- sf::st_sf(geometry = sf::st_geometry(x))

  elevation <- suppressMessages(
    elevatr::get_elev_point(points, src = "aws", z = zoom)$elevation
  )

  x$elevation <- as.numeric(elevation)
  x
}

add_elevation_to_point <- function(x, source, zoom) {
  if (source == "elevatr") {
    return(fwa_add_elevatr_elevation_to_point(x, zoom = zoom))
  }
  fwa_add_gm_elevation_to_point(x)
}

check_source <- function(
  source,
  fun,
  warn = TRUE,
  user_env = rlang::caller_env(2)
) {
  if (is.null(source)) {
    if (warn) {
      lifecycle::deprecate_soft(
        "0.0.2",
        I(paste0("Calling `", fun, "()` without specifying `source`")),
        details = paste0(
          "The default `source = \"google\"` is being used. ",
          "Please set `source = \"google\"` or `source = \"elevatr\"`."
        ),
        user_env = user_env
      )
    }
    source <- "google"
  }
  chk_string(source)
  chk_subset(source, c("elevatr", "google"))
  source
}
