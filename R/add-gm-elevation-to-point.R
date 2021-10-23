#' Add Google Maps Elevation to Point
#'
#' @param x An sf object of spatial points.
#' @param key A string of the google maps elevation api key.
#' @return An updated version of x with numeric column elevation.
#' @seealso `googleway::google_elevation()`
#' @export
#' @examples
#' \dontrun{
#' rm <- fwa_add_rms_to_blk(data.frame(blk = 356308001))
#' fwa_add_gm_elevation_to_point(rm)
#' }
fwa_add_gm_elevation_to_point <- function(x,
                                key = Sys.getenv("GOOGLE_MAPS_ELEVATION_API_KEY")) {

  if(!requireNamespace("googleway", quietly = TRUE)) {
    err("Package 'googleway' must be installed to get elevations from Google Maps.")
  }

  chk_s3_class(x, "sf")
  chk_s3_class(sf::st_geometry(x), "sfc_POINT")
  chk_string(key)

  if(!nrow(x)) {
    x$elevation <- numeric(0)
    return(x)
  }

  coords <- x |>
    sf::st_transform(crs = 4326) |>
    sf::st_coordinates() |>
    dplyr::as_tibble() |>
    dplyr::select(lon = .data$X, lat = .data$Y)

  elevation <- googleway::google_elevation(coords, key = key)$results$elevation
  if(is.null(elevation)) {
    err("Invalid Google Maps Elevation API key.")
  }

  x$elevation <- elevation
  x
}
