split_string_pairwise <- function(x, y) {
  chk::chk_s3_class(x, "sfc")
  chk::chk_s3_class(y, "sfc")

  chk_s3_class(x, "sfc_LINESTRING")
  chk_s3_class(y, "sfc_POINT")

  if(!length(x)) {
    return(x)
  }
  chk_identical(length(y), length(x))

  for(i in 1:length(x)) {
    suppressWarnings(x[i] <- lwgeom::st_split(x[i], y[i]) |>
      sf::st_collection_extract("LINESTRING"))
  }
  x
}
