group_split_sf <- function(.tbl, ..., .keep = TRUE) {
  is_sf <- inherits(.tbl, "sf")

  if(is_sf) {
    sf_column_name <- attr(.tbl, "sf_column")
  }
  x <- dplyr::group_split(.tbl, ..., .keep = .keep)
  if(is_sf) {
    x <- x |>
      lapply(sf::st_sf, sf_column_name = sf_column_name)
  }
  x
}

# https://stackoverflow.com/questions/43627679/round-any-equivalent-for-dplyr
round_any <- function(x, accuracy, f = round) {
  f(x/ accuracy) * accuracy
}

# https://stackoverflow.com/questions/12688717/round-up-from-5
round_up = function(x) {
  posneg <- sign(x)
  y <- abs(x)
  y <- y + 0.5 + sqrt(.Machine$double.eps)
  y <- trunc(y)
  y <- y * posneg
  as.integer(y)
}

is_try_error <- function(x) inherits(x, "try-error")

bbox <- function(x) {
  bbox <- sf::st_transform(x, 4326)
  sf::st_bbox(bbox)
}

divide_by <- function(x, y) {
  x / y
}

same_crs <- function(x, y) {
  sf::st_transform(x, crs = sf::st_crs(y))
}
