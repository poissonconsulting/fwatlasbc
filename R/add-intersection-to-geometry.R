get_intersection_geometry <- function(nm, x, y) {
  y <- y |>
    dplyr::filter(.data$name == nm)

  check_dim(y, nrow, 1L) # +chk

  x |>
    sf::st_intersects(y, sparse = FALSE) |>
    as.vector()
}

#' Add Intersection to Geometry
#'
#' Adds a logical column for each name in name in y
#' indicating whether each element of x intersects with the element of y.
#'
#' @param x An sf data frame.
#' @param y An sf data frame with character column name.
#' @return A copy of x with a logical column for each name in name
#' indicating whether each element of x intersects with the element of y.
#' @export
fwa_add_intersection_to_geometry <- function(x, y) {
  chk_s3_class(x, "sf")
  chk_s3_class(y, "sf")

  check_dim(x, nrow)

  check_names(y, "name")
  chk_not_subset(colnames(x), y$name)

  chk_character_or_factor(y$name)
  chk_not_any_na(y$name)
  chk_unique(y$name)
  chk_valid_name(y$name)

  if (!nrow(y)) return(x)

  splits <- lapply(y$name, get_intersection_geometry, x = x, y = y)
  names(splits) <- y$name
  splits <- dplyr::as_tibble(splits)

  if (!"tbl" %in% class(x)) {
    x <- dplyr::as_tibble(x)
  }

  x |>
    dplyr::bind_cols(splits)
}
