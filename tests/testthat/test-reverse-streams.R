make_reverse_streams <- function() {
  ls <- function(...) sf::st_linestring(matrix(c(...), ncol = 2, byrow = TRUE))
  sf::st_sf(
    blk = c(1L, 1L, 2L, 3L),
    id = 1:4,
    geometry = sf::st_sfc(
      ls(0, 0, 0, 500),
      ls(0, 500, 0, 1000, 0, 1500),
      ls(0, 400, 300, 400),
      ls(0, 700, 300, 700),
      crs = 3005
    )
  )
}

test_that("fwa_reverse_streams reverses specified streams", {
  streams <- make_reverse_streams()
  x <- fwa_reverse_streams(streams, c(1, 3))
  expect_s3_class(x, "sf")
  expect_identical(colnames(x), colnames(streams))
  expect_identical(x$id, streams$id)
  expect_identical(sf::st_crs(x), sf::st_crs(streams))
  expect_identical(
    sf::st_coordinates(x$geometry[[1]]),
    sf::st_coordinates(streams$geometry[[1]])[2:1, ]
  )
  expect_identical(
    sf::st_coordinates(x$geometry[[2]]),
    sf::st_coordinates(streams$geometry[[2]])[3:1, ]
  )
  expect_identical(x$geometry[[3]], streams$geometry[[3]])
  expect_identical(
    sf::st_coordinates(x$geometry[[4]]),
    sf::st_coordinates(streams$geometry[[4]])[2:1, ]
  )
})

test_that("fwa_reverse_streams twice is identity", {
  streams <- make_reverse_streams()
  x <- streams |>
    fwa_reverse_streams(2L) |>
    fwa_reverse_streams(2L)
  expect_identical(x, streams)
})

test_that("fwa_reverse_streams no blk", {
  streams <- make_reverse_streams()
  expect_identical(fwa_reverse_streams(streams, integer()), streams)
})

test_that("fwa_reverse_streams errors if blk not in x", {
  streams <- make_reverse_streams()
  expect_snapshot(fwa_reverse_streams(streams, c(1L, 4L)), error = TRUE)
})

test_that("fwa_reverse_streams errors if blk not unique", {
  streams <- make_reverse_streams()
  expect_error(fwa_reverse_streams(streams, c(1L, 1L)))
})

test_that("fwa_reverse_streams errors with invalid blk", {
  streams <- make_reverse_streams()
  expect_error(fwa_reverse_streams(streams, NA_integer_))
  expect_error(fwa_reverse_streams(streams, 1.5))
})

test_that("fwa_reverse_streams errors with non-linestrings", {
  streams <- make_reverse_streams() |>
    sf::st_cast("MULTILINESTRING")
  expect_error(fwa_reverse_streams(streams, 1L))
})
