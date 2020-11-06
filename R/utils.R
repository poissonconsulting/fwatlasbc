get_epsg <- function(x){
  chk_sf_sfc(x)
  sf::st_crs(x, parameters = TRUE)$epsg
}

is_try_error <- function(x) inherits(x, "try-error")

