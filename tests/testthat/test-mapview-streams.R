test_that("fwa_mapview_streams works", {
  ls <- function(...) sf::st_linestring(matrix(c(...), ncol = 2, byrow = TRUE))
  x <- sf::st_sf(
    blk = c(1L, 2L),
    geometry = sf::st_sfc(
      ls(1000000, 500000, 1000000, 501000),
      ls(1000500, 500500, 1000000, 500500),
      crs = 3005
    )
  )
  expect_s4_class(fwa_mapview_streams(x), "mapview")
  expect_s4_class(fwa_mapview_streams(x, layer = "blk"), "mapview")
  skip("need to test image snapshot")
})

test_that("fwa_mapview_streams errors with points", {
  x <- sf::st_sf(
    blk = 1L,
    geometry = sf::st_sfc(sf::st_point(c(1000000, 500000)), crs = 3005)
  )
  expect_error(fwa_mapview_streams(x))
})
