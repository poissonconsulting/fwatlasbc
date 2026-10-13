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

test_that("split_linestrings splits into pieces with position", {
  ls <- function(...) sf::st_linestring(matrix(c(...), ncol = 2, byrow = TRUE))
  x <- sf::st_sf(
    blk = c(1L, 2L),
    geometry = sf::st_sfc(
      ls(0, 0, 0, 100, 0, 400),
      ls(0, 0, 100, 0),
      crs = 3005
    )
  )
  y <- split_linestrings(x)
  expect_s3_class(y, "sf")
  expect_identical(y$blk, c(1L, 1L, 2L))
  expect_equal(y$..fwa_position, c(50 / 400, 250 / 400, 0.5))
  expect_identical(sf::st_crs(y), sf::st_crs(x))
  expect_identical(
    sf::st_coordinates(y$geometry[[2]])[, c("X", "Y")],
    matrix(c(0, 0, 100, 400), ncol = 2, dimnames = list(NULL, c("X", "Y")))
  )
})
