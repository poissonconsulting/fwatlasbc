add_gm_elevation_to_point <- function(x, digits, key) {
  coords <- x |>
    sf::st_transform(crs = 4326) |>
    sf::st_coordinates() |>
    dplyr::as_tibble() |>
    dplyr::select(lon = "X", lat = "Y") |>
    dplyr::mutate(lon = round(.data$lon, digits), lat = round(.data$lat, digits))

  elevation <- googleway::google_elevation(coords, key = key)$results$elevation
  if (is.null(elevation)) {
    err("Invalid Google Maps Elevation API key.")
  }
  x$elevation <- elevation
  x
}
#' Add Google Maps Elevation to Point
#'
#' @param x An sf object of spatial points.
#' @param chunk_size The number of rows to include in each API query.
#' @param digits The number of digits to round the latitude and longitude by
#' before querying the elevation from the API
#' @param key A string of the Google Maps Elevation API key.
#' @return An updated version of x with numeric column elevation.
#' @seealso `googleway::google_elevation()`
#' @export
#' @examples
#' \dontrun{
#' rm <- fwa_add_rms_to_blk(data.frame(blk = 356308001))
#' fwa_add_gm_elevation_to_point(rm)
#' }
fwa_add_gm_elevation_to_point <- function(x, chunk_size = 300L, digits = 7,
                                          key = Sys.getenv("GOOGLE_MAPS_ELEVATION_API_KEY")) {
  rlang::check_installed("googleway", reason = "to get elevations from Google Maps.")

  chk_s3_class(x, "sf")
  chk_s3_class(sf::st_geometry(x), "sfc_POINT")
  chk_not_subset(colnames(x), c("..fwa_chunk"))
  chk_whole_numeric(chunk_size)
  chk_gt(chunk_size)
  chk_whole_numeric(digits)
  chk_range(digits, c(0, 15))
  chk_string(key)

  if (!nrow(x)) {
    x$elevation <- numeric(0)
    return(x)
  }

  x$..fwa_chunk <- seq_len(nrow(x)) %/% chunk_size

  x |>
    group_split_sf(.data$..fwa_chunk) |>
    lapply(add_gm_elevation_to_point, digits = digits, key = key) |>
    dplyr::bind_rows() |>
    dplyr::select(!"..fwa_chunk")
}
