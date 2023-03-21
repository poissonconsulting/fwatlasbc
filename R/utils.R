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

has_name <- function(x, name) {
  name %in% names(x)
}

is.sf <- function (x) {
  inherits(x, "sf")
}

is_linestring <- function(x) {
  "LINESTRING" %in% class(x)
}

sample_linestring <- function(x, interval, end) {
  length <- sf::st_length(x) |> as.numeric()
  sample <- seq(0, length, by = interval)
  end <- length - sample[length(sample)] >= end

  if(end) {
    sample <- c(sample, length)
  }
  sample <- sample / length

  points <- sf::st_geometry(x) |>
    sf::st_line_sample(sample = sample) |>
    sf::st_cast("POINT")

  x <- x |>
    dplyr::slice(rep(1, length(points))) |>
    sf::st_set_geometry(points) |>
    dplyr::mutate(rm = (dplyr::row_number() - 1) * interval,
                  rm = as.integer(.data$rm))
  if(end) {
    x$rm[nrow(x)] <- as.integer(length)
  }
  x
}

sample_linestrings <- function(x, interval, end) {
  x <- x |>
    dplyr::mutate(..fwa_id = 1:dplyr::n()) |>
    dplyr::group_split(.data$..fwa_id) |>
    purrr::map(sample_linestring, interval, end = end) |>
    dplyr::bind_rows() |>
    dplyr::select(!"..fwa_id")
}

reverse_linestrings <- function(x) {
  crs <- sf::st_crs(x)

  linestrings <- sf::st_geometry(x) |>
    purrr::map(reverse_linestring) |>
    sf::st_sfc() |>
    sf::st_set_crs(crs)

  sf::st_set_geometry(x, linestrings)
}

reverse_linestring <- function(x) {
  sf::st_linestring(x[rev(seq_len(nrow(x))),])
}

sf_column_name <- function (x) {
  if (!is.sf(x) || is.null(attr(x, "sf_column"))) {
    return(character(0))
  }
  attr(x, "sf_column")
}

# from https://tanaylab.github.io/tgutil/index.html
pmean <- function(..., na.rm = FALSE) {
  d <- do.call(cbind, list(...))
  res <- rowMeans(d, na.rm = na.rm)
  idx_na <- !rowMeans(!is.na(d))
  res[idx_na] <- NA
  return(res)
}
