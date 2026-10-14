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
  expect_s4_class(fwa_mapview_streams(x, half_streams = TRUE), "mapview")
  expect_s4_class(
    fwa_mapview_streams(x, layer = "blk", half_streams = TRUE),
    "mapview"
  )
  skip("need to test image snapshot")
})

test_that("fwa_mapview_streams errors with points", {
  x <- sf::st_sf(
    blk = 1L,
    geometry = sf::st_sfc(sf::st_point(c(1000000, 500000)), crs = 3005)
  )
  expect_error(fwa_mapview_streams(x))
})

test_that("fwa_mapview_streams errors with streams column if half_streams", {
  ls <- function(...) sf::st_linestring(matrix(c(...), ncol = 2, byrow = TRUE))
  x <- sf::st_sf(
    blk = 1L,
    streams = "a",
    geometry = sf::st_sfc(ls(1000000, 500000, 1000000, 501000), crs = 3005)
  )
  expect_s4_class(fwa_mapview_streams(x), "mapview")
  expect_error(fwa_mapview_streams(x, half_streams = TRUE))
})

test_that("halve_linestrings splits into down and up halves", {
  ls <- function(...) sf::st_linestring(matrix(c(...), ncol = 2, byrow = TRUE))
  x <- sf::st_sf(
    blk = c(1L, 2L),
    geometry = sf::st_sfc(
      ls(0, 0, 0, 100, 0, 400),
      ls(0, 0, 100, 0),
      crs = 3005
    )
  )
  y <- halve_linestrings(x)
  expect_s3_class(y, "sf")
  expect_identical(y$blk, c(1L, 2L, 1L, 2L))
  expect_identical(y$streams, c("down", "down", "up", "up"))
  expect_identical(sf::st_crs(y), sf::st_crs(x))
  expect_equal(as.numeric(sf::st_length(y)), c(200, 50, 200, 50))
  expect_equal(
    unname(sf::st_coordinates(y$geometry[[1]])[, c("X", "Y")]),
    matrix(c(0, 0, 0, 0, 100, 200), ncol = 2)
  )
  expect_equal(
    unname(sf::st_coordinates(y$geometry[[3]])[, c("X", "Y")]),
    matrix(c(0, 0, 200, 400), ncol = 2)
  )
})
