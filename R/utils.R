is_try_error <- function(x) inherits(x, "try-error")

bbox <- function(x) {
  bbox <- sf::st_transform(x, 4326)
  sf::st_bbox(bbox)
}
