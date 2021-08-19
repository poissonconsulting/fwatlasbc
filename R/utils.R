get_epsg <- function(x){
  chk_sf_sfc(x)
  sf::st_crs(x, parameters = TRUE)$epsg
}

is_try_error <- function(x) inherits(x, "try-error")

bbox <- function(x) {
  bbox <- sf::st_transform(x, 4326)
  sf::st_bbox(bbox)
}
